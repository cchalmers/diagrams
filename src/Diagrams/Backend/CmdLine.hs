{-# LANGUAGE ConstrainedClassMethods     #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.CmdLine
-- Copyright   :  (c) 20132016 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient creation of command-line-driven executables for rendering
-- diagrams.  This module provides a general framework and default
-- behaviors for parsing command-line arguments, records for diagram
-- creation options in various forms, and classes and instances for a
-- unified entry point to command-line-driven diagram creation
-- executables.
--
-- For a tutorial on command-line diagram creation see
-- <http://projects.haskell.org/diagrams/doc/cmdline.html>.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.CmdLine
  (

    -- * Options

    -- ** Standard options
    DiagramOpts(..)
  -- , diagramOpts
  -- , output

    -- ** Multi-diagram options
  , OutputPath(..)
  , PickBackend(..)
  , DiagramMultiOpts(..)
  -- , diagramMultiOpts
  -- , selection
  -- , list

    -- ** Animation options
  , DiagramAnimOpts(..)
  -- , diagramAnimOpts
  -- , fpu

    -- ** Loop options
  , DiagramLoopOpts(..)
  -- , diagramLoopOpts
  -- , loop
  -- , src
  -- , interval

    -- * Parsing
  , Parseable(..)
  , readHexColor

    -- * Command-line programs (@Mainable@)
    -- ** Arguments, rendering, and entry point
  , Mainable(..)

    -- ** General currying
  , ToResult(..)

    -- ** helper functions for implementing @mainRender@
  -- , defaultAnimMainRender
  , defaultMultiMainRender
  , defaultLoopRender
  ) where

-- import           Control.Lens              (Lens', (&), (.~), (^.))
import           Options.Applicative
import           Options.Applicative.Types (readerAsk)
import           Control.Monad             (forever, unless, when)
-- import           Text.Printf
-- import           Data.Active               hiding (interval)
import           Data.Char                 (isDigit)
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.SRGB
import           Data.Data
import           Data.IORef
import           Data.List                 (delete)
import           Data.Maybe                (fromMaybe)
import           Data.Monoid
import           Numeric
import           Control.Concurrent        (threadDelay)
import           System.Directory          (canonicalizePath)
import           System.Environment        (getArgs, getProgName)
import           System.Exit               (ExitCode (..))
import           System.FilePath           ( -- addExtension, splitExtension,
                                            replaceExtension, dropExtension,
                                            takeDirectory, takeFileName, (</>))
import           System.FSNotify           (WatchConfig (..), defaultConfig,
                                            eventTime, watchDir,
                                            withManagerConf)
import           System.FSNotify.Devel     (existsEvents)
import           System.Info               (os)
import           System.IO                 (hFlush, stdout)
import           System.Process            (readProcessWithExitCode)

import Geometry.Size
import Geometry.TwoD.Size
import Geometry.TwoD.Types
--
import           Diagrams.Animation
import           Diagrams.Attributes
import           Diagrams.Types
import           Diagrams.Util

-- | A hidden \"helper\" option which always fails.
--   Taken from Options.Applicative.Extra but without the
--   short option 'h'.  We want the 'h' for Height.
helper' :: Parser (a -> a)
helper' = abortOption ShowHelpText $ mconcat
  [long "help", short '?', help "Show this help text"]

-- | Apply a parser to the command line that includes the standard
--   program description and help behavior.  Results in parsed commands
--   or fails with a help message.
defaultOpts :: Parser a -> IO a
defaultOpts optsParser = do
  prog <- getProgName
  let p = info (helper' <*> optsParser) $
            mconcat [fullDesc, progDesc "Command-line diagram generation.", header prog]
  execParser p

------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------

-- | Parseable instances give a command line parser for a type.  If a
--   custom parser for a common type is wanted a newtype wrapper could
--   be used to make a new 'Parseable' instance.  Notice that we do
--   /not/ want as many instances as 'Read' because we want to limit
--   ourselves to things that make sense to parse from the command line.
class Parseable a where
  -- | Default command line parser for the type.
  parser :: Parser a

  default parser :: Read a => Parser a
  -- | The @default@ parser uses the 'Read' instance for that type.
  parser = argument auto mempty

instance (Read n, Num n) => Parseable (SizeSpec V2 n) where
  parser = mkSizeSpec2D <$> o w <*> o h where
    o = optional . option auto
    w = mconcat [ long "width", short 'w', metavar "INT"
                , help "Desired width of the output image"]
    h = mconcat [ long "height", short 'h', metavar "INT"
                , help "Desired height of the output image"]

-- | 'Read' instance.
instance Parseable Int

-- |  'Read' instance.
instance Parseable Double where

-- | Parse a string by just accepting the given string.
instance Parseable String where
  parser = argument str mempty

-- | Parse @'Colour' Double@ as either a named color from "Data.Colour.Names"
--   or a hexadecimal color.
instance Parseable (Colour Double) where
  parser = argument (rc <|> rh) mempty
    where
      rh, rc :: ReadM (Colour Double)
      rh = f . colorToSRGBA <$> (readerAsk >>= readHexColor)
      rc = readerAsk >>= readColourName
      f (r,g,b,_) = sRGB r g b
      -- TODO: this seems unfortunate. Should the alpha value be applied
      -- to the r g b values?

-- | Parse @'AlphaColour' Double@ as either a named color from "Data.Colour.Names"
--   or a hexadecimal color.
instance Parseable (AlphaColour Double) where
  parser = argument (rc <|> rh) mempty
    where
      rh = readerAsk >>= readHexColor
      rc = opaque <$> (readerAsk >>= readColourName)

-- Addapted from the Clay.Color module of the clay package

-- | Parses a hexadecimal color.  The string can start with @\"0x\"@ or @\"#\"@
--   or just be a string of hexadecimal values.  If four or three digits are
--   given each digit is repeated to form a full 24 or 32 bit color.  For
--   example, @\"0xfc4\"@ is the same as @\"0xffcc44\"@.  When eight or six
--   digits are given each pair of digits is a color or alpha channel with the
--   order being red, green, blue, alpha.
readHexColor :: (Applicative m, Monad m) => String -> m (AlphaColour Double)
readHexColor cs = case cs of
  ('0':'x':hs) -> handle hs
  ('#':hs)     -> handle hs
  hs           -> handle hs
  where
    handle hs | length hs <= 8 && all isHexDigit hs = case hs of
      [a,b,c,d,e,f,g,h] -> withOpacity <$> (sRGB <$> hex a b <*> hex c d <*> hex e f) <*> hex g h
      [a,b,c,d,e,f    ] -> opaque      <$> (sRGB <$> hex a b <*> hex c d <*> hex e f)
      [a,b,c,d        ] -> withOpacity <$> (sRGB <$> hex a a <*> hex b b <*> hex c c) <*> hex d d
      [a,b,c          ] -> opaque      <$> (sRGB <$> hex a a <*> hex b b <*> hex c c)
      _                 -> fail $ "could not parse as a colour" ++ cs
    handle _ = fail $ "could not parse as a colour: " ++ cs

    isHexDigit c = isDigit c || c `elem` "abcdef"

    hex a b = (/ 255) <$> case readHex [a,b] of
                [(h,"")] -> return h
                _        -> fail $ "could not parse as a hex value" ++ [a,b]

instance Parseable () where
  parser = pure ()

instance (Parseable a, Parseable b) => Parseable (a,b) where
  parser = (,) <$> parser <*> parser

instance (Parseable a, Parseable b, Parseable c) => Parseable (a, b, c) where
  parser = (,,) <$> parser <*> parser <*> parser

instance (Parseable a, Parseable b, Parseable c, Parseable d) => Parseable (a, b, c, d) where
  parser = (,,,) <$> parser <*> parser <*> parser <*> parser

newtype PickBackend = PickBackend { getBackendName :: Maybe FilePath }

instance Parseable PickBackend where
  parser = PickBackend <$> optional (strOption opts) where
    opts = mconcat [ long "backend", short 'b', metavar "BACKEND"
                   , help "Name of the backend to use" ]

newtype OutputPath = OutputPath { getOutput :: FilePath }

instance Parseable OutputPath where
  parser = OutputPath <$> strOption opts where
    opts = mconcat [long "output", short 'o', metavar "PATH", help "Output file"]

-- | Standard options most diagrams are likely to have.
--
--   Command line parser for 'DiagramOpts':
--     - Width is option @--width@ or @-w@.
--     - Height is option @--height@ or @-h@ (note we change help to be
--       @-?@ due to this).
--     - Output is option @--output@ or @-o@.
data DiagramOpts = DiagramOpts
  { _optsSizeSpec :: SizeSpec V2 Int
  , _output       :: FilePath
  }
  deriving Typeable

instance Parseable DiagramOpts where
  parser = DiagramOpts <$> parser <*> outputParser where
    outputParser = strOption $ mconcat
      [long "output", short 'o', metavar "PATH", help "Output file"]

-- | Extra options for a program that can offer a choice between
--   multiple diagrams.
--
--   Command line parser for 'DiagramMultiOpts':
--     - Selection is option @--selection@ or @-S@.
--     - List is @--list@ or @-L@.
data DiagramMultiOpts = DiagramMultiOpts
  { _selection :: Maybe String -- ^ Selected diagram to render.
  , _list      :: Bool         -- ^ Flag to indicate that a list of available diagrams should
                               --   be printed to standard out.
  }
  deriving (Show, Data, Typeable)

instance Parseable DiagramMultiOpts where
  parser = DiagramMultiOpts <$> name <*> list where
    name = optional . strOption $
      mconcat [ long "selection", short 'S', metavar "NAME"
              , help "NAME of the diagram to render"]
    list = switch $
      mconcat [ long "list", short 'L'
              , help "List all available diagrams"]

-- | Extra options for animations.
--
--   Command line parser for 'DiagramAnimOpts':
--     - Frames per time unit is @--fpu@ or @-f@.
data DiagramAnimOpts = DiagramAnimOpts
  { _fpu :: Double
  }
  deriving (Show, Data, Typeable)

instance Parseable DiagramAnimOpts where
  parser = DiagramAnimOpts <$> frames where
    frames = option auto $ mconcat
      [ long "fpu", short 'f', value 30.0
      , help "Frames per unit time (for animations)"]

-- | Extra options for command-line looping.
--
--   CommandLine parser for 'DiagramLoopOpts':
--     - Loop is @--loop@ or @-l@.
--     - Source is @--src@ or @-s@.
--     - Interval is @-i@ defaulting to one second.
data DiagramLoopOpts = DiagramLoopOpts
  { _loop     :: Bool            -- ^ Flag to indicate that the program should loop creation.
  , _src      :: Maybe FilePath  -- ^ File path for the source file to recompile.
  , _interval :: Int             -- ^ Interval in seconds at which to check for recompilation.
  }

instance Parseable DiagramLoopOpts where
  parser = DiagramLoopOpts <$> switch loop <*> src <*> int where
    loop = mconcat [long "loop", short 'l', help "Run in a self-recompiling loop"]
    src  = optional . strOption $ mconcat [long "src", short 's', help "Source file to watch"]
    int  = option auto $ mconcat
        [ long "interval", short 'i', value 1, metavar "INT"
        , help "When running in a loop, check for changes every i seconds."]

-- To result class -----------------------------------------------------

-- | This class allows us to abstract over functions that take some
--   arguments and produce a final value. When some @d@ is an instance
--   of 'ToResult' we get a type @'Args' d@ that is a type of /all/ the
--   arguments at once, and a type @'ResultOf' d@ that is the type of
--   the final result from some base case instance.
class ToResult d where

  -- | The arguments needed to make the result.
  type Args d
  type Args d = ()

  -- | The result from using the 'Args'
  type ResultOf d
  type ResultOf d = d

  -- | Using the provided args, make a result.
  toResult :: d -> Args d -> ResultOf d
  default toResult :: d -> () -> d
  toResult d () = d

-- | A diagram can always produce a diagram when given @()@ as an
--   argument. This is our base case.
instance ToResult (QDiagram v n Any)

-- | A list of diagrams can produce pages.
instance ToResult [QDiagram v n Any]

-- | A list of named diagrams can give the multi-diagram interface.
instance ToResult [(String, QDiagram v n Any)]

-- | An animation is another suitable base case.
instance ToResult (Animation v)

-- | Diagrams that require IO to build are a base case.
instance ToResult d => ToResult (IO d) where
  type Args (IO d) = Args d
  type ResultOf (IO d) = IO (ResultOf d)

  toResult d args = flip toResult args <$> d

-- | An instance for a function that, given some 'a', can produce a 'd'
--   that is also an instance of 'ToResult'. For this to work we need
--   both the argument 'a' and all the arguments that 'd' will need.
--   Producing the result is simply applying the argument to the
--   producer and passing the remaining arguments to the produced
--   producer.

--   The previous paragraph stands as a witness to the fact that Haskell
--   code is clearer and easier to understand then paragraphs in English
--   written by me.
instance ToResult d => ToResult (a -> d) where
  type Args (a -> d) = (a, Args d)
  type ResultOf (a -> d) = ResultOf d

  toResult f (a,args) = toResult (f a) args

-- Mainable class ------------------------------------------------------

-- | This class represents the various ways we want to support diagram
--   creation from the command line. It has the right instances to
--   select between creating single static diagrams, multiple static
--   diagrams, static animations, and functions that produce diagrams as
--   long as the arguments are 'Parseable'.
--
--   Backends are expected to create @Mainable@ instances for the types
--   that are suitable for generating output in the backend's format.
--   For instance, Postscript can handle single diagrams, pages of
--   diagrams, animations as separate files, and association lists.
--   This implies instances for @Diagram Postscript R2@, @[Diagram
--   Postscript R2]@, @Animation Postscript R2@, and @[(String,Diagram
--   Postscript R2)]@.  We can consider these as the base cases for the
--   function instance.
--
--   The associated type 'MainOpts' describes the options which need to
--   be parsed from the command-line and passed to @mainRender@.
class Parseable (MainOpts d) => Mainable d where
  -- | Associated type that describes the options which need to be parsed
  -- from the command-line and passed to @mainRender@.
  type MainOpts d

  -- | This method invokes the command-line parser resulting in an
  --   options value or ending the program with an error or help
  --   message. Typically the default instance will work. If a different
  --   help message or parsing behavior is desired a new implementation
  --   is appropriate.
  --
  --   Note the @d@ argument should only be needed to fix the type @d@.
  --   Its value should not be relied on as a parameter.
  mainArgs :: proxy d -> IO (MainOpts d)
  mainArgs _ = defaultOpts parser

  -- | Backend specific work of rendering with the given options and
  --   mainable value is done here. All backend instances should
  --   implement this method.
  mainRender :: MainOpts d -> d -> IO ()

  -- | Main entry point for command-line diagram creation. This is the
  --   method that users will call from their program @main@. For
  --   instance an expected user program would take the following form.
  --
  -- @
  -- import Diagrams.Prelude
  -- import Diagrams.Backend.TheBestBackend.CmdLine
  --
  -- d :: Diagram B
  -- d = ...
  --
  -- main = mainWith d
  -- @
  --
  --   Most backends should be able to use the default implementation.
  --   A different implementation should be used to handle more complex
  --   interactions with the user.
  mainWith :: d -> IO ()
  mainWith d = do
    opts <- mainArgs (Just d)
    mainRender opts d

-- | This instance allows functions resulting in something that is 'Mainable' to
--   be 'Mainable'.  It takes a parse of collected arguments and applies them to
--   the given function producing the 'Mainable' result.
instance (ToResult d, Parseable a, Parseable (Args d), Mainable (ResultOf d))
    => Mainable (a -> d) where
  type MainOpts (a -> d) = (MainOpts (ResultOf (a -> d)), Args (a -> d))

  mainRender (opts, a) f = mainRender opts (toResult f a)

-- | With this instance we can perform IO to produce something
--   'Mainable' before rendering.
instance Mainable d => Mainable (IO d) where
  type MainOpts (IO d) = MainOpts d

  mainRender opts dio = dio >>= mainRender opts

-- | @defaultMultiMainRender@ is an implementation of 'mainRender' where
--   instead of a single diagram it takes a list of diagrams paired with names
--   as input.  The generated executable then takes a @--selection@ option
--   specifying the name of the diagram that should be rendered.  The list of
--   available diagrams may also be printed by passing the option @--list@.
--
--   Typically a backend can write its @[(String,QDiagram v n Any)]@ instance as
--
--   @
--   instance Mainable [(String,QDiagram v n Any)] where
--       type MainOpts [(String,QDiagram v n Any)] = (DiagramOpts, DiagramMultiOpts)
--       mainRender = defaultMultiMainRender
--   @
--
--   We do not provide this instance in general so that backends can choose to
--   opt-in to this form or provide a different instance that makes more sense.
defaultMultiMainRender
  :: Mainable d
  => (MainOpts d, DiagramMultiOpts)
  -> [(String, d)]
  -> IO ()
defaultMultiMainRender (opts,multi) ds =
  if _list multi
    then showDiaList (map fst ds)
    else case _selection multi of
           Nothing  -> putStrLn "No diagram selected." >> showDiaList (map fst ds)
           Just sel -> case lookup sel ds of
                         Nothing -> putStrLn $ "Unknown diagram: " ++ sel
                         Just d  -> mainRender opts d

-- | Display the list of diagrams available for rendering.
showDiaList :: [String] -> IO ()
showDiaList ds = do
  putStrLn "Available diagrams:"
  putStrLn $ "  " ++ unwords ds

-- | @defaultAnimMainRender@ is an implementation of 'mainRender' which
--   renders an animation as numbered frames, named by extending the
--   given output file name by consecutive integers.  For example if the
--   given output file name is @foo\/blah.ext@, the frames will be saved
--   in @foo\/blah001.ext@, @foo\/blah002.ext@, and so on (the number of
--   padding digits used depends on the total number of frames).  It is
--   up to the user to take these images and stitch them together into
--   an actual animation format (using, /e.g./ @ffmpeg@).
--
--   Of course, this is a rather crude method of rendering animations;
--   more sophisticated methods will likely be added in the future.
--
--   The @fpu@ option from 'DiagramAnimOpts' can be used to control how
--   many frames will be output for each second (unit time) of
--   animation.
--
--   This function requires a lens into the structure that the
--   particular backend uses for it's diagram base case.  If @MainOpts
--   (QDiagram b v n Any) ~ DiagramOpts@ then this lens will simply be
--   'output'.  For a backend supporting looping
--   it will most likely be @_1 . output@.  This lens is required
--   because the implementation works by modifying the output field and
--   running the base @mainRender@.  Typically a backend can write its
--   @Animation B V@ instance as
--
--   @
--   instance Mainable (Animation B V) where
--       type MainOpts (Animation B V) = (DiagramOpts, DiagramAnimOpts)
--       mainRender = defaultAnimMainRender output
--   @
--
--   We do not provide this instance in general so that backends can
--   choose to opt-in to this form or provide a different instance that
--   makes more sense.

-- defaultAnimMainRender ::
--     (opts -> QDiagram b v n Any -> IO ())
--     -> Lens' opts FilePath -- ^ A lens into the output path.
--     -> (opts, DiagramAnimOpts)
--     -> Animation v n
--     -> IO ()
-- defaultAnimMainRender renderF out (opts,animOpts) anim = do
--   let frames  = simulate (toRational $ animOpts^.fpu) anim
--       nDigits = length . show . length $ frames
--   forM_ (zip [1..] frames) $ \(i,d) -> renderF (indexize out nDigits i opts) d

-- | @indexize d n@ adds the integer index @n@ to the end of the
--   output file name, padding with zeros if necessary so that it uses
--   at least @d@ digits.
-- indexize :: Lens' s FilePath -> Int -> Integer -> s -> s
-- indexize out nDigits i opts = opts & out .~ output'
--   where fmt         = "%0" ++ show nDigits ++ "d"
--         output'     = addExtension (base ++ printf fmt i) ext
--         (base, ext) = splitExtension (opts^.out)

-- | Put a string and flush. Usefull for progress updates that don't
--   contain a newline.
putStrF :: String -> IO ()
putStrF s = putStr s >> hFlush stdout

-- | Default way to render with looping turned on.
defaultLoopRender :: DiagramLoopOpts -> IO ()
defaultLoopRender opts = when (_loop opts) $ do
  putStrLn "Looping turned on"
  prog <- getProgName
  args <- getArgs

  srcPath <- case _src opts of
    Just path -> return path
    Nothing   -> fromMaybe (error nosrc) <$> findHsFile prog
      where
        nosrc = "Unable to find Haskell source file.\n"
             ++ "Specify source file with '-s' or '--src'"
  srcPath' <- canonicalizePath srcPath

  sandbox     <- findSandbox []
  sandboxArgs <- case sandbox of
    Nothing -> return []
    Just sb -> do
      putStrLn ("Using sandbox " ++ takeDirectory sb)
      return ["-package-db", sb]

  let args'       = delete "-l" . delete "--loop" $ args
      newProg     = newProgName (takeFileName srcPath) prog
      timeOfDay   = take 8 . drop 11 . show . eventTime

  -- Polling is only used on Windows
  withManagerConf defaultConfig { confPollInterval = _interval opts } $
    \mgr -> do
      lock <- newIORef False

      _ <- watchDir mgr (takeDirectory srcPath') (existsEvents (== srcPath'))
        $ \ev -> do
          running <- atomicModifyIORef lock ((,) True)
          unless running $ do
            putStrF ("Modified " ++ timeOfDay ev ++ " ... ")
            exitCode <- recompile srcPath' newProg sandboxArgs
            -- Call the new program without the looping option
            run newProg args' exitCode
            atomicWriteIORef lock False

      putStrLn $ "Watching source file " ++ srcPath
      putStrLn $ "Compiling target: " ++ newProg
      putStrLn $ "Program args: " ++ unwords args'
      forever . threadDelay $ case os of
         -- https://ghc.haskell.org/trac/ghc/ticket/7325
        "darwin" -> 5000000000000
        _        -> maxBound

recompile :: FilePath -> FilePath -> [String] -> IO ExitCode
recompile srcFile outFile args = do
  let ghcArgs = ["--make", srcFile, "-o", outFile] ++ args
  putStrF "compiling ... "
  (exit, _, stderr) <- readProcessWithExitCode "ghc" ghcArgs ""
  when (exit /= ExitSuccess) $ putStrLn ('\n':stderr)
  return exit

-- | On Windows, the next compilation must have a different output
--   than the currently running program.
newProgName :: FilePath -> String -> String
newProgName srcFile oldName = case os of
  "mingw32" ->
      if oldName == replaceExtension srcFile "exe"
        then replaceExtension srcFile ".1.exe"
        else replaceExtension srcFile "exe"
  _ -> dropExtension srcFile

-- | Run the given program with specified arguments, if and only if
--   the previous command returned ExitSuccess.
run :: String -> [String] -> ExitCode -> IO ()
run prog args ExitSuccess = do
  let path = "." </> prog
  putStrF "running ... "
  (exit, stdOut, stdErr) <- readProcessWithExitCode path args ""
  case exit of
    ExitSuccess   -> putStrLn "done."
    ExitFailure r -> do
      putStrLn $ prog ++ " failed with exit code " ++ show r
      unless (null stdOut) $ putStrLn "stdout:" >> putStrLn stdOut
      unless (null stdErr) $ putStrLn "stderr:" >> putStrLn stdErr
run _ _ _ = return ()

