{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Diagrams.Backend
  ( module Diagrams.Backend
  , module Diagrams.Backend.CmdLine
  ) where

import qualified Options.Applicative      as OP

import           Data.Char                (toLower)
import           Data.List                (find, intercalate, isSuffixOf)
import           System.Directory         (getDirectoryContents)
import           System.FilePath          (takeExtension)
import           System.IO.Unsafe         (unsafePerformIO)

import           Geometry.Envelope
import           Geometry.Size
import           Geometry.Space
import           Geometry.Transform
import           Geometry.TwoD.Types

import           Control.Lens             (Lens', each, over, _1)
import           Diagrams.Types
import           Diagrams.Util

import           Diagrams.Backend.CmdLine

-- | Backends are used to render diagrams.
class Backend b where

  -- | The result from rendering a diagram.
  type Result b

  -- | The options used to render a diagram. This normally includes the
  --   output size of the diagram.
  data Options b

  -- | Render a diagram into the result along with the transformation
  --   from the original diagram to output diagram.
  renderDiaT :: Options b -> Diagram (V b) -> (V2 Double, Transformation (V b) Double, Result b)

  -- | General information about the backend that can be used by various
  --   other tools and libraries.
  backendInfo :: b -> BackendInfo

-- 2D helpers ----------------------------------------------------------

-- | Adjust the size and position of a 2D diagram to fit within the
--   requested size. Returns the extent of the final diagram, any
--   transformation applied to the diagram (the inverse of which can be
--   used, say, to translate output/device coordinates back into local
--   diagram coordinates), and the modified diagram itself.
adjustSize2D
  :: SizeSpec V2 Int
  -> Diagram V2
  -> (V2 Double, T2 Double, Diagram V2)
adjustSize2D spec d = (sz, t, d # transform t)
  where
    (sz, t) = sizeAdjustment (fmap fromIntegral spec) (boundingBox d)

adjustSize
  :: HasLinearMap v
  => SizeSpec v Int
  -> Diagram v
  -> (v Double, Transformation v Double, Diagram v)
adjustSize spec d = (sz, t, d # transform t)
  where
    (sz, t) = sizeAdjustment (fmap fromIntegral spec) (boundingBox d)


-- Generical output ----------------------------------------------------

-- | Backends that can render do a file with a 2D size output.
--
--   To be used to diagrams-builder
class Backend b => BackendBuild b where

  -- | Render a diagram to a file with the given options.
  saveDiagram'
    :: FilePath      -- ^ diagram options
    -> Options b     -- ^ output file
    -> Diagram (V b) -- ^ diagram to render
    -> IO ()         -- ^ render

  -- | Construct suitable default options given a 2D size (in pixels).
  mkOptions :: SizeSpec V2 Int -> Options b

  -- | Lens onto the size spec.
  sizeSpec :: Lens' (Options b) (SizeSpec V2 Int)

  -- | Show the options in a way that may be interpreted by ghc
  --   verbatim. If the show instance is derived, this can just be 'show'.
  showOptions :: Options b -> String

-- | Render a diagram to a file for a specific backend, using the
--   default options.
saveDiagram
  :: BackendBuild b
  => b               -- ^ backend token
  -> FilePath        -- ^ output path
  -> SizeSpec V2 Int -- ^ size of diagram
  -> Diagram (V b)   -- ^ diagram to render
  -> IO ()
saveDiagram b path sz = saveDiagram' path (mkOptionsFor b sz)

-- | Make options for a specific backend.
mkOptionsFor :: BackendBuild b => b -> SizeSpec V2 Int -> Options b
mkOptionsFor _ = mkOptions

-- | Information needed to generate code to make a diagram.
data BackendInfo = BackendInfo
  { backendModuleName :: String
  , backendTokenName  :: String
  , backendModules    :: [String]
    -- ^ modules that need to be imported for the token to be in scope
  , backendNames      :: [String]
    -- ^ names that can be used to refer to this backend. all lower
    -- case, dashes used beween words
  , backendExtensions :: [String]
    -- ^ extensions supported by this backend (all lower case)
  }

officialBackends :: [BackendInfo]
officialBackends = [cairoInfo, pgfInfo, svgInfo, rasterificInfo]

pgfInfo :: BackendInfo
pgfInfo = BackendInfo
  { backendModuleName = "diagrams-pgf"
  , backendTokenName  = "PGF"
  , backendModules    = ["Diagrams.Backend.PGF"]
  , backendNames      = ["pgf", "tikz", "tex", "latex", "context", "pdf"]
  , backendExtensions = ["pgf", "tikz", "portable-graphics-format"]
  }

rasterificInfo :: BackendInfo
rasterificInfo = BackendInfo
  { backendModuleName  = "diagrams-rasterific"
  , backendTokenName   = "Rasterific"
  , backendModules     = ["Diagrams.Backend.Rasterific"]
  , backendNames       = ["raterific", "raster"]
  , backendExtensions  = ["pdf", "jpg", "jpeg", "gif", "tiff", "png"]
  }

cairoInfo :: BackendInfo
cairoInfo = BackendInfo
  { backendModuleName  = "diagrams-cairo"
  , backendTokenName   = "Cairo"
  , backendModules     = ["Diagrams.Backend.Cairo"]
  , backendNames       = ["svg", "raster", "scalable-graphics-format"]
  , backendExtensions  = ["pdf", "jpg", "jpeg", "gif", "tiff", "png"]
  }

svgInfo :: BackendInfo
svgInfo = BackendInfo
  { backendModuleName  = "diagrams-svg"
  , backendTokenName   = "SVG"
  , backendModules     = ["Diagrams.Backend.SVG"]
  , backendNames       = ["svg", "raster", "scalable-graphics-format"]
  , backendExtensions  = ["svg"]
  }

sdlInfo :: BackendInfo
sdlInfo = BackendInfo
  { backendModuleName  = "diagrams-sdl"
  , backendTokenName   = "SDL"
  , backendModules     = ["Diagrams.Backend.SDL"]
  , backendNames       = ["sdl"]
  , backendExtensions  = []
  }

canvasInfo :: BackendInfo
canvasInfo = BackendInfo
  { backendModuleName = "diagrams-canvas"
  , backendTokenName  = "PGF"
  , backendModules    = ["Diagrams.Backend.Canvas"]
  , backendNames      = ["canvas"]
  , backendExtensions = []
  }

povrayInfo :: BackendInfo
povrayInfo = BackendInfo
  { backendModuleName = "diagrams-povray"
  , backendTokenName  = "POVRay"
  , backendModules    = ["Diagrams.Backend.POVRay"]
  , backendNames      = ["povray"]
  , backendExtensions = []
  }


-- renderCode
--   :: BackendInfo -- ^ backend attemping to use
--   -> FilePath    -- ^ output file
--   -> SizeSpec V2 Int -- ^ output size
--   -> String      -- ^ name of diagram to render
--   -> Either String String
-- renderCode bi outpath sz nm
--   | not (backendNameMatch bi (tail $ takeExtension outpath)) = Left "extension not supported" -- XXX replace tail with something safe
--   | not (backendModuleName bi `elem` currentPackages)        = Left "package not in sandbox paths"
--   | otherwise = Right $ "Diagrams.Backend.renderDiaToFileFor " ++
--       backendTokenName bi ++ " " ++ outpath ++ " " ++ showSize sz ++ nm

showSize :: SizeSpec V2 Int -> String
showSize = undefined
  -- V2 (Just x) (Just y) -> "dims2D x y"
  -- V2 (Just x) Nothing  -> "mkWidth x"
  -- V2 Nothing (Just y)  -> "mkHeight y"
  -- V2 Nothing Nothing   -> "absolute"


-- Packages shoudln't change while running the program.
currentPackages :: [String]
currentPackages = unsafePerformIO currentPackagesIO

currentPackagesIO :: IO [String]
currentPackagesIO = do
  msb <- findSandbox []
  x <- case msb of
         Just sb -> pure sb
         Nothing -> globalPackage
  filter (isSuffixOf ".conf.d") <$> getDirectoryContents x

data MBack v where
  MBack :: (V b ~ v, BackendBuild b) => b -> MBack v

showBacks :: [(String, MBack v)] -> String
showBacks [] = "No avalable backends"
showBacks bs = intercalate ", " $ map fst bs

chooseBackend :: String -> [(String, MBack v)] -> Maybe (MBack v)
chooseBackend nm = lookup nm . over (each._1.each) toLower

-- | Some backend that can render a 2D diagram to a file given a
--   'Filepath' and a 'SizeSpec'
data SomeBackend = SomeBackend
  { someBackendNames    :: [String]
  , supportedExtensions :: [String]
  , someRender          :: FilePath -> SizeSpec V2 Int -> Diagram V2 -> IO ()
    -- it's possible to add a parser in here if we really want
  }

instance RenderOutcome SomeBackend (Diagram V2) where
  type MainOpts SomeBackend (Diagram V2) = (FilePath, SizeSpec V2 Int)
  resultParser _ _ = (,) <$> outputParser <*> sizeParser
  renderOutcome sb (path,sz) = someRender sb path sz

-- | The name of the backend to use for rendering.
backendParser :: OP.Parser (Maybe String)
backendParser = OP.optional . OP.strOption $ mconcat
  [ OP.long "backend", OP.short 'b', OP.metavar "STRING"
  , OP.help "Name of the backend to use" ]
  -- We could add a completer to this.

instance RenderOutcome [SomeBackend] (Diagram V2) where
  type MainOpts [SomeBackend] (Diagram V2) = (Maybe String, FilePath, SizeSpec V2 Int)
  resultParser _ _ = (,,) <$> backendParser <*> outputParser <*> sizeParser
  renderOutcome [] _ = error "No provided backends!"
  renderOutcome bs (mBack,path,sz) = someRender sb path sz
    where
      ext = takeExtension path
      sb  = case mBack of
        Just nm -> case find (\b -> nm `elem` someBackendNames b) bs of
          Just b -> b
          Nothing -> error $ "backend " ++ show nm ++ "not found"
        Nothing -> case find (\b -> ext `elem` supportedExtensions b) bs of
          Just b -> b
          Nothing -> case ext of
            "" -> error "empty output extension, cannot infer file type"
            e  -> error $ "unsupported extension " ++ show e

-- | Create a 'SomeBackend' using the 'BackendBuild' class to render for
--   that backend.
someBackend :: (BackendBuild b, V b ~ V2) => b -> SomeBackend
someBackend b = SomeBackend
  { someBackendNames    = backendNames info
  , supportedExtensions = backendExtensions info
  , someRender          = saveDiagram b
  } where info = backendInfo b

