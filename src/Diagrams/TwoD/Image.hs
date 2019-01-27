{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Image
-- Copyright   :  (c) 2011-2016 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Importing external images into diagrams.  Usage example: To create
-- a diagram from an embedded image with width 1 and height set
-- according to the aspect ratio, use @image img # scaleUToX 1@, where
-- @img@ is a value of type @DImage n e@, created with a function like
-- 'loadImageEmb', 'loadImageExt', or 'raster'.
-----------------------------------------------------------------------------

module Diagrams.TwoD.Image
  ( -- * Embedded images
    imageEmb
  , imageEmbBS
  , loadImageEmb
  , rasterDia

    -- * External images
  , imageExtUnchecked
  , loadImageExt

    -- * Internals
  , DImage (..)
  , Embedded
  , External
  , Native
  , dimage
  , dimageSize
  , loadImageSize

  , generateImageRGBA

    -- ** Converting between image types
  , externalToEmbedded
  , embeddedToExternal
  ) where

import           Codec.Picture
import           Codec.Picture.Types  (dynamicMap)
import           Control.Lens         ((<&>))

import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Data.Char            (toLower)
import           Data.Colour          (AlphaColour)
import           Data.Semigroup
import           Data.Typeable        (Typeable)
import           System.FilePath      (takeExtension)

import           Geometry.BoundingBox
import           Geometry.Envelope
import           Geometry.Query
import           Geometry.Space
import           Geometry.Trace
import           Geometry.TwoD.Types

import           Diagrams.Attributes  (colorToSRGBA)
import           Diagrams.Types

-- | Native images are backend specific image that allow their own
--   format.
data Native t deriving Typeable

-- | A diagrams images.
data DImage :: * -> * where
  -- | An embedded JuicyPixels image. See 'loadImageEmb' and
  --   'loadImageEmbBS'
  ImageEmbedded :: DynamicImage -> DImage Embedded

  -- | A reference to an external image along with its size. Backends
  --   whose target supports images as references will keep this image
  --   as a reference. To get the size of the image automatically use
  --   'loadImageExt'.
  ImageExternal :: V2 Int -> FilePath -> DImage External

  -- | Images that are specialised to a particular backend.
  ImageNative :: V2 Int -> t -> DImage (Native t)
  deriving Typeable

dimageSize :: DImage t -> V2 Int
dimageSize = \case
  ImageEmbedded img -> V2 (dynamicMap imageWidth img) (dynamicMap imageHeight img)
  ImageExternal s _ -> s
  ImageNative s _   -> s

type instance V (DImage a) = V2
type instance N (DImage a) = Double

instance HasQuery (DImage a) Any where
  getQuery = getQuery . boundingBox
  {-# INLINE getQuery #-}

instance Enveloped (DImage a) where
  getEnvelope = getEnvelope . boundingBox
  {-# INLINE getEnvelope #-}

  boundingBox (dimageSize -> V2 w h) = fromCorners (mkP2 (-w') (-h')) (mkP2 w' h')
    where
      w' = fromIntegral w / 2; h' = fromIntegral h / 2
  {-# INLINE boundingBox #-}

instance Traced (DImage a) where
  getTrace = getTrace . boundingBox
  {-# INLINE getTrace #-}

-- | Make a 'DImage' into a 'Diagram'.
dimage :: Typeable a => DImage a -> Diagram V2
dimage = primQD

-- Embedded images -----------------------------------------------------

-- | Embedded images contain the raw data is imbedded in the output file
--   of the diagram. Internally they are represented by a 'DynamicImage'
--   from the JuicyPixels library.
data Embedded deriving Typeable

-- | Crate a diagram from raw raster data.
rasterDia
  :: Int -- ^ width
  -> Int -- ^ height
  -> (Int -> Int -> AlphaColour Double) -- ^ generating function
  -> Diagram V2
rasterDia w h f = dimage $ ImageEmbedded (ImageRGBA8 (generateImageRGBA w h f))

-- | Create an image "from scratch" by specifying the pixel data.
generateImageRGBA
  :: Int -- ^ width
  -> Int -- ^ height
  -> (Int -> Int -> AlphaColour Double) -- ^ generating function
  -> Image PixelRGBA8
generateImageRGBA w h f = img
  where
    img = generateImage f' w h
    f' x y = fromAlphaColour $ f x y

    fromAlphaColour c = PixelRGBA8 r g b a where
      (r, g, b, a) = (int r', int g', int b', int a')
      (r', g', b', a') = colorToSRGBA c
      int x = round (255 * x)

-- | Create an embedded image from a 'DynamicImage'.
imageEmb :: DynamicImage -> Diagram V2
imageEmb = dimage . ImageEmbedded

-- | Use JuicyPixels to decode a bytestring into an image. This will work out which kind of image to 
imageEmbBS :: ByteString -> Either String (Diagram V2)
imageEmbBS path = imageEmb <$> decodeImage path

-- | Use JuicyPixels to read an image in any format and use it as an
--   'Embedded' image in a diagram.  The width and height of the image
--   are set to their actual values.
loadImageEmb :: FilePath -> IO (Either String (Diagram V2))
loadImageEmb path = fmap imageEmb <$> readImage path

-- External images -----------------------------------------------------

-- | External images point to an image file 'FilePath'. The file formats
--   supported depends on the backend.
data External deriving Typeable

-- | Get the size of an image, returning an error if there was a
--   problem.
loadImageSize :: FilePath -> IO (Either String (V2 Int))
loadImageSize path = do
  -- It would be much more efficient to only look at the image header to
  -- get the size but JuicyPixels doesn't seem to have an easy way to do
  -- this.
  eimg <- readImage path
  pure (dimageSize . ImageEmbedded <$> eimg)

-- | Check that a file exists, and use JuicyPixels to figure out
--   the right size, but save a reference to the image instead
--   of the raster data
loadImageExt :: FilePath -> IO (Either String (Diagram V2))
loadImageExt path = do
  isize <- loadImageSize path
  pure $ isize <&> \sz -> dimage $ ImageExternal sz path

-- | Make an "unchecked" image reference; have to specify a
--   width and height. Unless the aspect ratio of the external
--   image is the w :: h, then the image will be distorted.
imageExtUnchecked :: Int -> Int -> FilePath -> Diagram V2
imageExtUnchecked w h path = dimage $ ImageExternal (V2 w h) path

-- | Convert an external image to an embedded one. The old size is
-- ignored.
externalToEmbedded :: DImage External -> IO (Either String (DImage Embedded))
externalToEmbedded (ImageExternal _ path) = fmap ImageEmbedded <$> readImage path

-- | Convert an embedded image to an external one, saving the image to
--   the path. This can be useful for backends that don't support
--   embedded images. The type of image used depends on the extension of
--   the output file.
embeddedToExternal :: FilePath -> DImage Embedded -> IO (Either String (DImage External))
embeddedToExternal path dimg@(ImageEmbedded dyn) =
  case map toLower $ takeExtension path of
    ".png" -> writeDynamicPng path dyn >> exImg

    ".jpg" -> case dyn of
      ImageYCbCr8 img -> LB.writeFile path (encodeJpeg img) >> exImg
      _               -> pure $ Left "jpeg can only encode ImageYcbCr8 images"

    ".jpeg" -> case dyn of
      ImageYCbCr8 img -> LB.writeFile path (encodeJpeg img) >> exImg
      _               -> pure $ Left "jpeg can only encode ImageYcbCr8 images"

    ".tiff" -> case dyn of
      ImageY8 img     -> LB.writeFile path (encodeTiff img) >> exImg
      ImageY16 img    -> LB.writeFile path (encodeTiff img) >> exImg
      ImageYA8 img    -> LB.writeFile path (encodeTiff img) >> exImg
      ImageRGB8 img   -> LB.writeFile path (encodeTiff img) >> exImg
      ImageRGB16 img  -> LB.writeFile path (encodeTiff img) >> exImg
      ImageRGBA8 img  -> LB.writeFile path (encodeTiff img) >> exImg
      ImageRGBA16 img -> LB.writeFile path (encodeTiff img) >> exImg
      ImageCMYK8 img  -> LB.writeFile path (encodeTiff img) >> exImg
      ImageCMYK16 img -> LB.writeFile path (encodeTiff img) >> exImg
      _               -> pure $ Left "Unsupported image format for TIFF export"

    '.':ext -> pure $ Left ("Unknown file extension: " ++ ext)

    _       -> pure $ Left "No file extension given"

  where exImg = pure . Right $ ImageExternal (dimageSize dimg) path
