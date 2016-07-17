{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Diagrams.Backend
  ( module Diagrams.Backend
  , module Diagrams.Backend.CmdLine
  ) where

-- import Data.Semigroup
import Data.Reflection
import Data.Proxy
import Data.Maybe (fromMaybe)
-- import Data.Monoid.Action
-- import Data.Monoid.WithSemigroup
-- import Control.Applicative
import Data.Char (toLower)
import System.FilePath (takeExtension)
import System.Directory (getDirectoryContents)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (isSuffixOf)

import Geometry.Size
import Geometry.Envelope
import Geometry.Transform
import Geometry.TwoD.Types
import Geometry.Space

import Control.Lens (Lens')
import Diagrams.Types
import Diagrams.Util

import Diagrams.Backend.CmdLine
import Data.Constraint
import Data.Constraint.Unsafe

-- | Backends are used to render diagrams.
class Backend b where

  -- | The result from rendering a diagram.
  type Result b

  -- | The options used to render a diagram. This normally includes the
  --   output size of the diagram.
  data Options b

  -- | Render a diagram into the result along with the transformation
  --   from the original diagram to output diagram.
  renderDiaT :: Options b -> Diagram (V b) -> (Result b, Transformation (V b) Double)

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

-- Generical output ----------------------------------------------------

-- | Backends that can render do a file with a 2D size output.
--
--   To be used to diagrams-builder
class Backend b => BackendBuild b where

  -- | Render a diagram to a file with the given options.
  saveDiagram'
    :: Options b     -- ^ output file
    -> FilePath      -- ^ diagram options
    -> Diagram (V b) -- ^ diagram to render
    -> IO ()         -- ^ render

  -- | Construct suitable default options given a 2D size (in pixels).
  mkOptions :: SizeSpec V2 Int -> Options b

  -- | Lens onto the size spec.
  sizeSpec :: Lens' (Options b) (SizeSpec V2 Int)

-- | Render a diagram to a file for a specific backend, using the
--   default options.
saveDiagram
  :: BackendBuild b
  => b               -- ^ backend token
  -> SizeSpec V2 Int -- ^ size of diagram
  -> FilePath        -- ^ output path
  -> Diagram (V b)   -- ^ diagram to render
  -> IO ()
saveDiagram b sz = saveDiagram' (mkOptionsFor b sz)

-- | Make options for a specific backend.
mkOptionsFor :: BackendBuild b => b -> SizeSpec V2 Int -> Options b
mkOptionsFor _ = mkOptions

-- | Information needed to generate code to make a diagram.
data BackendInfo = BackendInfo
  { backendModuleName  :: String
  , backendTokenName   :: String
  , backendModules     :: [String]
  , backendNameMatch   :: String -> Bool
  , extensionSupported :: FilePath -> Bool
    -- ^ does the backend support
  }

officialBackends :: [BackendInfo]
officialBackends = [cairoInfo, pgfInfo, svgInfo, rasterificInfo]

pgfInfo :: BackendInfo
pgfInfo = BackendInfo
  { backendModuleName  = "diagrams-pgf"
  , backendTokenName   = "PGF"
  , backendModules     = ["Diagrams.Backend.PGF"]
  , backendNameMatch   = (`elem` pgfNames) . map toLower
  , extensionSupported = (`elem` extensions) . map toLower
  }
  where
    extensions = ["pgf", "tikz", "tex", "latex", "context", "pdf"]
    pgfNames   = ["pgf", "tikz", "portable-graphics-format"]

rasterificInfo :: BackendInfo
rasterificInfo = BackendInfo
  { backendModuleName  = "diagrams-rasterific"
  , backendTokenName   = "Rasterific"
  , backendModules     = ["Diagrams.Backend.Rasterific"]
  , backendNameMatch   = (`elem` rasterNames) . map toLower
  , extensionSupported = (`elem` extensions) . map toLower
  }
  where
    extensions  = ["pdf", "jpg", "jpeg", "gif", "tiff", "png"]
    rasterNames = ["raterific", "raster"]

cairoInfo :: BackendInfo
cairoInfo = BackendInfo
  { backendModuleName  = "diagrams-cairo"
  , backendTokenName   = "Cairo"
  , backendModules     = ["Diagrams.Backend.Cairo"]
  , backendNameMatch   = (`elem` cairoNames) . map toLower
  , extensionSupported = (`elem` extensions) . map toLower
  }
  where
    extensions  = ["pdf", "jpg", "jpeg", "gif", "tiff", "png"]
    cairoNames = ["svg", "raster", "scalable-graphics-format"]

svgInfo :: BackendInfo
svgInfo = BackendInfo
  { backendModuleName  = "diagrams-svg"
  , backendTokenName   = "SVG"
  , backendModules     = ["Diagrams.Backend.SVG"]
  , backendNameMatch   = (`elem` svgNames) . map toLower
  , extensionSupported = (`elem` extensions) . map toLower
  }
  where
    extensions  = ["svg"]
    svgNames = ["svg", "raster", "scalable-graphics-format"]

renderCode
  :: BackendInfo -- ^ backend attemping to use
  -> FilePath    -- ^ output file
  -> SizeSpec V2 Int -- ^ output size
  -> String      -- ^ name of diagram to render
  -> Either String String
renderCode bi outpath sz nm
  | not (backendNameMatch bi (tail $ takeExtension outpath)) = Left "extension not supported" -- XXX replace tail with something safe
  | not (backendModuleName bi `elem` currentPackages)        = Left "package not in sandbox paths"
  | otherwise = Right $ "Diagrams.Backend.renderDiaToFileFor " ++
      backendTokenName bi ++ " " ++ outpath ++ " " ++ showSize sz ++ nm

showSize :: SizeSpec V2 Int -> String
showSize = undefined


-- Packages shoudln't change while running the program.
currentPackages :: [String]
currentPackages = unsafePerformIO currentPackagesIO
{-# INLINE currentPackages #-}

currentPackagesIO :: IO [String]
currentPackagesIO = do
  msb <- findSandbox []
  x <- case msb of
         Just sb -> pure sb
         Nothing -> globalPackage
  filter (isSuffixOf ".conf.d") <$> getDirectoryContents x

-- Command line --------------------------------------------------------

-- class Parsable (MainOpts d) => Mainable d where
--   type MainOpts d

--   mainArgs :: proxy d -> IO (MainOpts d)

--   mainRender :: MainOpts d -> d -> IO ()

-- newtype MainDiagram b v n = MainDiagram (Diagram v n Any)

-- mkMain :: b -> QDiagram v n m -> MainDiagram b v n
-- mkMain _ d = MainDiagram $ mempty <$ d

-- instance (BackendBuild b v n, Parsable (Options b v n)) => Mainable (MainDiagram b v n) where
--   type MainOpts (MainDiagram b v n) = (Options b v n, OutputPath)

--   -- mainArgs _ = defaultOpts parser
--   mainRender (opts, outpath) (MainDiagram d) = renderFile outpath opts d

-- instance ToResult (MainDiagram b v n) where
--   type Args (MainDiagram b v n) = ?
--   type ResultOf (MainDiagram b v n) = ?

--   toResult d ? = ?d

-- instance (Given b, Mainable (MainDiagram b v n)) => ToResult (Diagram v n) where
--   type MainOpts (Diagram v n) = ()
--   type ResultOf (Diagram v n) = Diagram v n

-- -- Building without custom options -------------------------------------

-- Give us a mainable instance for a diagram using the mainsble instance
-- for d.
underiveMainable
  :: Coercible (Diagram v) d
  => (b -> Diagram v -> d) -> b -> Mainable d :- Mainable (Diagram v)
underiveMainable f b = unsafeUnderive (f b)


-- Dumb version that uses default options for the size spec.
newtype SimpleBuild b = SimpleBuild (Diagram (V b))

instance BackendBuild b => Mainable (SimpleBuild b) where
  type MainOpts (SimpleBuild b) = (DiagramOpts, DiagramLoopOpts)

  mainRender (diaOpts, loopOpts) (SimpleBuild d) = do
    let opts = mkOptions $ _optsSizeSpec diaOpts :: Options b
    saveDiagram' opts (_output diaOpts) d
    defaultLoopRender loopOpts
    -- case saveDiagram' opts (_output diaOpts) d of
    --   Left e  -> putStrLn $ "diagram render error:" ++ e
    --   Right r -> r >> defaultLoopRender loopOpts

mkSimpleBuildFor :: b -> Diagram (V b) -> SimpleBuild b
mkSimpleBuildFor _ = SimpleBuild

simpleMainWith
  :: BackendBuild b
  => b -> (Mainable (Diagram (V b)) => d) -> d
simpleMainWith b d = d \\ underiveMainable mkSimpleBuildFor b

-- Slightly less dumb version that uses the parser for the backend's
-- options.
newtype DefaultBuild b = DefaultBuild (Diagram (V b))

instance (Parseable (Options b), BackendBuild b) => Mainable (DefaultBuild b) where
  type MainOpts (DefaultBuild b) = (Options b, OutputPath, DiagramLoopOpts)

  mainRender (diaOpts, outpath, loopOpts) (DefaultBuild d) = do
    saveDiagram' diaOpts (getOutput outpath) d
    defaultLoopRender loopOpts
    -- case saveDiagram' diaOpts (getOutput outpath) d of
    --   Left e  -> putStrLn $ "diagram render error:" ++ e
    --   Right r -> r >> defaultLoopRender loopOpts

mkDefaultBuildFor :: b -> Diagram (V b) -> DefaultBuild b
mkDefaultBuildFor _ = DefaultBuild

-- | Give a mainable instance for a diagram using the given backend.
--
--   Can be used in conjuction with 'mainWith':
--
-- @
-- withBackend SVG mainWith $ \c -> circle 3 # fc c # frame 1
-- @
withBackend
  :: (Parseable (Options b), BackendBuild b)
  => b -> (Mainable (Diagram (V b)) => d) -> d
withBackend b d = d \\ underiveMainable mkDefaultBuildFor b
  -- (unsafeUnderive (mkDefaultBuildFor b) :: Mainable (DefaultBuild b) :- Mainable (Diagram V2))

-- | Default main for the given backend.
defaultMain :: (Parseable (Options b), BackendBuild b) => b -> Diagram (V b) -> IO ()
defaultMain b = mainWith . mkDefaultBuildFor b

-- Multibackends -------------------------------------------------------

-- Muliple backends at once, using the simple render for it.
-- newtype SomeBackend v n = SomeBackend (forall b. BackendBuild b v n => b)
data SomeBackend v where
  SomeBackend :: (V b ~ v, BackendBuild b) => b -> SomeBackend v

someRender
  :: SomeBackend v
  -> SizeSpec V2 Int
  -> FilePath
  -> Diagram v
  -> IO ()
someRender sb sz path dia = case sb of
  SomeBackend b -> saveDiagram b sz path dia

newtype Backends v = Backends [(String, SomeBackend v)]

-- | A tagged diagram. Used for reflecting a backend.
newtype TDiagram s v = TDiagram (Diagram v)

multiDia :: proxy s -> Diagram v -> TDiagram s v
multiDia _ = TDiagram

multiSubs :: Proxy s -> Mainable (TDiagram s v) :- Mainable (Diagram v)
multiSubs _ = unsafeUnderive TDiagram

instance Reifies s (Backends v) => Mainable (TDiagram s v) where
  type MainOpts (TDiagram s v) = (PickBackend, DiagramOpts, DiagramLoopOpts)

  mainRender (bName, diaOpts, loopOpts) (TDiagram d) = do
    let Backends bs = reflect (Proxy @ s)
        backupB = snd $ head bs -- XXX do something better
        b = fromMaybe backupB $ do
              nm <- getBackendName bName
              lookup nm bs
    someRender b (_optsSizeSpec diaOpts) (_output diaOpts) d
    defaultLoopRender loopOpts

-- | Produce a 'Mainable' instance for a diagram by picking a backend.
multiBackend :: [(String, SomeBackend v)] -> (Mainable (Diagram v) => d) -> d
multiBackend bs d = reify (Backends bs) (\p -> d \\ underiveMainable multiDia p)

-- | A safe version of 'multiBackend' that provides the list of backends
--   via @s@. The proxy is to be used in conjunction with 'multiDia'.
multiBackendP
  :: [(String, SomeBackend v)]
  -> (forall s. Mainable (TDiagram s v) => Proxy s -> d)
  -> d
multiBackendP bs d = reify (Backends bs) d

