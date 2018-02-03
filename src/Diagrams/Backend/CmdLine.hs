{-# LANGUAGE ConstrainedClassMethods   #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.CmdLine
-- Copyright   :  (c) 2013-2016 diagrams team (see LICENSE)
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

    -- * Command-line programs (@Mainable@)
    -- ** Arguments, rendering, and entry point
    Mainable
  , mainable

  , MainWith
  , mainWith

    -- ** Looping
  , loopMainWith

  , RenderOutcome (..)
  , WithOutcome (..)

  -- * Helpers
  , defaultExecParser
  , helper'
  , DiagramLoopOpts(..)

    -- ** Parsers
  , fpuParser
  , outputParser
  , sizeParser
  , loopOptsParser

    -- * Argument parsing
  , Parseable (..)
  , readHexColor

  ) where

import           Control.Concurrent        (threadDelay)
import           Control.Monad             (forever, unless, when)
import           Data.Char                 (isDigit)
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.SRGB
import           Data.Data
import           Data.Functor.Identity
import           Data.IORef
import           Data.List                 (delete)
import           Data.Maybe                (fromMaybe)
import           Numeric
import           Options.Applicative
import           Options.Applicative.Types (readerAsk)
import           System.Directory          (canonicalizePath)
import           System.Environment        (getArgs, getProgName)
import           System.Exit               (ExitCode (..))
import           System.FilePath           (dropExtension, replaceExtension,
                                            takeDirectory, takeFileName, (</>))
import           System.FSNotify           (WatchConfig (..), defaultConfig,
                                            eventTime, watchDir,
                                            withManagerConf)
import           System.FSNotify.Devel     (existsEvents)
import           System.Info               (os)
import           System.IO                 (hFlush, stdout)
import           System.Process            (readProcessWithExitCode)

import           Geometry.Size
-- import           Geometry.Space
import           Geometry.TwoD.Size
import           Geometry.TwoD.Types

import           Diagrams.Animation
import           Diagrams.Attributes
import           Diagrams.Types
import           Diagrams.Util

-- | A hidden \"helper\" option which always fails.  Taken from
--   "Options.Applicative.Extra" but without the short option 'h'.  We
--   want the 'h' for Height.
helper' :: Parser (a -> a)
helper' = abortOption ShowHelpText $ mconcat
  [long "help", short '?', help "Show this help text"]

-- | Apply a parser to the command line that includes the standard
--   program description and help behavior. Outcomes in parsed commands
--   or fails with a help message.
defaultExecParser :: Parser a -> IO a
defaultExecParser optsParser = do
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
  argRead :: ReadM a

  default argRead :: Read a => ReadM a
  -- | The @default@ parser uses the 'Read' instance for that type.
  argRead = auto

-- | 'Read' instance.
instance Parseable Int

-- |  'Read' instance.
instance Parseable Double where

-- | Parse a string by just accepting the given string.
instance Parseable String where
  argRead = str

-- | Parse @'Colour' Double@ as either a named color from "Data.Colour.Names"
--   or a hexadecimal color.
instance Parseable (Colour Double) where
  argRead = rc <|> rh
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
  argRead = rc <|> rh
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
  argRead = pure ()

instance (Parseable a, Parseable b) => Parseable (a,b) where
  argRead = (,) <$> argRead <*> argRead

instance (Parseable a, Parseable b, Parseable c) => Parseable (a, b, c) where
  argRead = (,,) <$> argRead <*> argRead <*> argRead

instance (Parseable a, Parseable b, Parseable c, Parseable d) => Parseable (a, b, c, d) where
  argRead = (,,,) <$> argRead <*> argRead <*> argRead <*> argRead

-- Mainable helper parsers ---------------------------------------------

-- | Frames per unit time: @--fpu 60@. Defaults to 30.
fpuParser :: Parser Rational
fpuParser = option auto $ mconcat
  [ long "fpu", value 30
  , help "Frames per unit time (for animations)"]

-- | Parser for the output file: @--output output.ext@ or @-o output.ext@.
outputParser :: Parser FilePath
outputParser = strOption $ mconcat
  [ long "output", short 'o', metavar "PATH"
  , help "Output file" ]

sizeParser :: Parser (SizeSpec V2 Int)
sizeParser = mkSizeSpec2D <$> o w <*> o h where
  o = optional . option auto
  w = mconcat [ long "width", short 'w', metavar "INT"
              , help "Desired width of the output image" ]
  h = mconcat [ long "height", short 'h', metavar "INT"
              , help "Desired height of the output image" ]

-- Mainable class ------------------------------------------------------

-- | Perform an IO action
class WithOutcome a where
  -- | Associated type that describes the options which need to be parsed
  -- from the command-line and passed to @mainRender@.
  type Args a
  type Args a = ()

  -- | The target type that eventually gets targeted with an 'IO' action in
  --   'withOutcome'.
  type Outcome a
  type Outcome a = a

  -- | The parser used to parse the arguments of @a@.
  argsParser :: proxy a -> Parser (Args a)

  -- | Default case with no arguments
  default argsParser :: Args a ~ () => proxy a -> Parser (Args a)
  argsParser _ = pure ()

  -- | Take an IO action that uses the Outcome of @a@ along with the
  --   args of a and @a@ itself and run the action.
  withOutcome :: (Outcome a -> IO ()) -> Args a -> a -> IO ()

  -- | Default case where @a@ is the target.
  default withOutcome :: (Args a ~ (), a ~ Outcome a)
                      => (Outcome a -> IO ()) -> Args a -> a -> IO ()
  withOutcome f () r = f r

resultProxy :: a -> Proxy (Outcome a)
resultProxy _ = Proxy

instance WithOutcome a => WithOutcome (IO a) where
  type Args (IO a)    = Args a
  type Outcome (IO a) = Outcome a
  argsParser _ = argsParser (Proxy :: Proxy a)
  withOutcome f args ioa = ioa >>= withOutcome f args

instance (Parseable a, WithOutcome b) => WithOutcome (a -> b) where
  type Args (a -> b)   = (a, Args b)
  type Outcome (a -> b) = Outcome b
  argsParser _ = (,) <$> argument argRead mempty <*> argsParser (Proxy :: Proxy b)
  withOutcome f (a, args) g = withOutcome f args (g a)

-- | Choose from a list of things to render.
instance WithOutcome a => WithOutcome [(String, a)] where
  type Args [(String, a)]   = (Maybe String, Args a)
  type Outcome [(String, a)] = Outcome a
  argsParser _ = (,) <$> nameParser <*> argsParser (Proxy :: Proxy a)
    where
    nameParser = optional . strOption $ mconcat [long "name",short 'n']
    -- It would be nice to add a completer to this parser but it's not
    -- possible with this setup.
  withOutcome f (mname, args) ds =
    case mname of
      Just nm -> case lookup nm ds of
        Just r  -> withOutcome f args r
        Nothing -> putStrLn $ "Option \"" ++ nm ++ "\" not found. Choose from:\n" ++ dsList
      Nothing -> putStrLn $ "Pick an option with --name. Choose from:\n" ++ dsList
    where
    dsList = unlines (map (("  "++) . fst) ds)

-- Terminating cases

instance WithOutcome ()
instance WithOutcome (Diagram v)

-- | Lists of diagrams go on multiple pages (supported backends only).
instance WithOutcome [Diagram v]

-- | Backends choose their own way to render animations.
instance WithOutcome (Animation v)

-- | Types whose target is the unit @()@. These can be used to construct
--   a main function with 'mainable'.
class (Outcome a ~ (), WithOutcome a) => Mainable a
instance (Outcome a ~ (), WithOutcome a) => Mainable a

-- | Run a main function where the target is the unit @()@, getting any
--   arguments from the command line.
--
-- @
-- 'mainable' :: 'IO' ()                 -> 'IO' ()
-- 'mainable' :: ('FilePath' -> 'IO' ()) -> 'IO' ()
-- 'mainable' :: [(String, 'IO' ())]     -> 'IO' ()
-- @
mainable :: Mainable a => a -> IO ()
mainable a = do
  args <- defaultExecParser (argsParser (Identity a))
  withOutcome return args a

-- Rendering results ---------------------------------------------------

-- | The 'Outcome' r can be rendered using @t@ and some options obtained
--   from the command line.
class RenderOutcome t r where
  -- | Options obtained through command line arguments which are used to
  --   render @r@.
  type MainOpts t r

  -- | The 'Parser' used to obtain the arguments from the command line.
  resultParser :: t -> proxy r -> Parser (MainOpts t r)
  default resultParser
    :: Parseable (MainOpts t r)
    => t -> proxy r -> Parser (MainOpts t r)
  resultParser _ _ = argument argRead mempty

  -- | Way to render @r@ given @t@ and the options recieved from the
  --   command line.
  renderOutcome :: t -> MainOpts t r -> r -> IO ()

-- | Combination of 'WithOutcome and 'RenderOutcome' where the 'Outcome of
--   @a@ can be rendered using the tag @t@.
class (RenderOutcome t (Outcome a), WithOutcome a) => MainWith t a
instance (RenderOutcome t (Outcome a), WithOutcome a) => MainWith t a

-- | Render @a@ using the token @t@. For diagrams the token is the
--   backend token (@SVG@, @PGF@ etc.)
--
--   For rendering diagrams with backend token @b@, we can make a simple
--   main than renders a plan diagram:
--
--   @
--   'mainWith' :: SVG -> 'Diagram' 'V2' -> 'IO' ()
--   @
--
--   But 'mainRender' can also take 'Parseable' arguments and 'IO'
--   'Diagram's:
--
--   @
--   'mainWith' :: b -> ('Colour' 'Double' -> 'Diagram' 'V2') -> 'IO' ()
--   'mainWith' :: b -> ('IO' ('Diagram' V2)) -> 'IO' ()
--   'mainWith' :: b -> ('FilePath' -> 'IO' ('Diagram' 'V2')) -> 'IO' ()
--   @
--
mainWith :: MainWith t a => t -> a -> IO ()
mainWith t a = do
  let parse = (,) <$> argsParser (Identity a)
                  <*> resultParser t (resultProxy a)
  (args,opts) <- defaultExecParser parse
  withOutcome (renderOutcome t opts) args a

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
-- defaultMultiMainRender
--   :: Mainable d
--   => (MainOpts d, DiagramMultiOpts)
--   -> [(String, d)]
--   -> IO ()
-- defaultMultiMainRender (opts,multi) ds =
--   if _list multi
--     then showDiaList (map fst ds)
--     else case _selection multi of
--            Nothing  -> putStrLn "No diagram selected." >> showDiaList (map fst ds)
--            Just sel -> case lookup sel ds of
--                          Nothing -> putStrLn $ "Unknown diagram: " ++ sel
--                          Just d  -> mainRender opts d

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

------------------------------------------------------------------------
-- Loop rendering
------------------------------------------------------------------------
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

loopOptsParser :: Parser DiagramLoopOpts
loopOptsParser = DiagramLoopOpts <$> switch loop <*> src <*> int where
    loop = mconcat [long "loop", short 'l', help "Run in a self-recompiling loop"]
    src  = optional . strOption $ mconcat [long "src", short 's', help "Source file to watch"]
    int  = option auto $ mconcat
        [ long "interval", short 'i', value 1, metavar "INT"
        , help "When running in a loop, check for changes every i seconds (Windows only)."]

-- | Similar to 'mainWith' but includes an additional @--loop@ option
--   (see 'loopOptsParser') that will recompile the module each time the
--   source of the diagram is saved.
loopMainWith :: MainWith t a => t -> a -> IO ()
loopMainWith t a = do
  let parse = (,,) <$> argsParser (Identity a)
                   <*> resultParser t (resultProxy a)
                   <*> loopOptsParser
  (args,opts,loopOpts) <- defaultExecParser parse
  withOutcome (renderOutcome t opts) args a
  defaultLoopRender loopOpts

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
        nosrc = "Unable to find Haskell source file for loop render.\n"
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

-- | Put a string and flush. Usefull for progress updates that don't
--   contain a newline.
putStrF :: String -> IO ()
putStrF s = putStr s >> hFlush stdout

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

