{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

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
  ( -- * DImage dtype
    DImage(..)
  , ImageData(..)
  , image

    -- * Embedded images
  , Embedded
  , embedded
  , embeddedDia
  , raster
  , rasterDia
  , loadImageEmb

    -- * External images
  , External
  , loadImageExt
  , uncheckedImageRef

    -- * Native images
  , Native

    -- * Converting between image types
  , externalToEmbedded
  , embeddedToExternal
  ) where

import           Codec.Picture
import           Codec.Picture.Types  (dynamicMap)
-- import Control.Lens hiding (transform)

import           Data.Colour          (AlphaColour)
import           Data.Semigroup
import           Data.Typeable        (Typeable)
import           Data.Char (toLower)
import           System.FilePath (takeExtension)
import qualified Data.ByteString.Lazy as LB

import           Geometry.Space
import           Geometry.BoundingBox
import           Geometry.Envelope
import           Geometry.Trace
import           Geometry.Query
import           Geometry.TwoD.Types

import           Diagrams.Attributes  (colorToSRGBA)
import           Diagrams.Types

-- import           Linear.Affine

-- | Native images are backend specific image that allow their own
--   format.
data Native t deriving Typeable

-- | 'ImageData' is either a JuicyPixels @DynamicImage@ tagged as 'Embedded' or
--   a reference tagged as 'External'. Additionally 'Native' is provided for
--   external libraries to hook into.
data ImageData :: * -> * where
  ImageRaster :: DynamicImage -> ImageData Embedded
  ImageRef    :: FilePath -> ImageData External
  ImageNative :: t -> ImageData (Native t)

-- | An image primitive, the two ints are width followed by height.
--   Will typically be created by @loadImageEmb@ or @loadImageExt@
--   which, will handle setting the width and height to the actual width
--   and height of the image.
data DImage :: * -> * where
  DImage :: !Int -> !Int -> ImageData t -> DImage t
  deriving Typeable

type instance V (DImage a) = V2
type instance N (DImage a) = Double

instance HasQuery (DImage a) Any where
  getQuery = getQuery . boundingBox
  {-# INLINE getQuery #-}

instance Enveloped (DImage a) where
  getEnvelope = getEnvelope . boundingBox
  {-# INLINE getEnvelope #-}

  boundingBox (DImage w h _) = fromCorners (mkP2 (-w') (-h')) (mkP2 w' h')
    where
      w' = fromIntegral w / 2; h' = fromIntegral h / 2
  {-# INLINE boundingBox #-}

instance Traced (DImage a) where
  getTrace = getTrace . boundingBox
  {-# INLINE getTrace #-}

-- | Make a 'DImage' into a 'Diagram'.
image :: Typeable a => DImage a -> Diagram V2
image = primQD

-- Embedded images -----------------------------------------------------

-- | Embedded images contain the raw data is imbedded in the output file
--   of the diagram. Internally they are represented by a 'DynamicImage'
--   from the "JuicyPixels" library.
data Embedded deriving Typeable

-- | Crate a diagram from raw raster data.
rasterDia
  :: Int -- ^ width
  -> Int -- ^ height
  -> (Int -> Int -> AlphaColour Double) -- ^ generating function
  -> Diagram V2
rasterDia w h f = image $ raster w h f

-- | Create an image "from scratch" by specifying the pixel data.
raster
  :: Int -- ^ width
  -> Int -- ^ height
  -> (Int -> Int -> AlphaColour Double) -- ^ generating function
  -> DImage Embedded
raster w h f = DImage w h (ImageRaster (ImageRGBA8 img))
  where
    img = generateImage f' w h
    f' x y = fromAlphaColour $ f x y

    fromAlphaColour c = PixelRGBA8 r g b a where
      (r, g, b, a) = (int r', int g', int b', int a')
      (r', g', b', a') = colorToSRGBA c
      int x = round (255 * x)

-- | Create an embedded image from a 'DynamicImage'.
embedded :: DynamicImage -> DImage Embedded
embedded img = DImage w h (ImageRaster img)
  where
    w = dynamicMap imageWidth img
    h = dynamicMap imageHeight img

-- | Create an embedded image from a 'DynamicImage'.
embeddedDia :: DynamicImage -> Diagram V2
embeddedDia = image . embedded

-- | Use JuicyPixels to read an image in any format and wrap it in a 'DImage'.
--   The width and height of the image are set to their actual values.
loadImageEmb :: FilePath -> IO (Either String (DImage Embedded))
loadImageEmb path = do
  dImg <- readImage path
  return $ case dImg of
    Left msg  -> Left msg
    Right img -> Right $ DImage w h (ImageRaster img)
      where
        w = dynamicMap imageWidth img
        h = dynamicMap imageHeight img

-- External images -----------------------------------------------------

-- | External images point to an image file 'FilePath'. The file formats
--   supported depends on the backend.
data External deriving Typeable

-- | Check that a file exists, and use JuicyPixels to figure out
--   the right size, but save a reference to the image instead
--   of the raster data
loadImageExt :: FilePath -> IO (Either String (DImage External))
loadImageExt path = do
  dImg <- readImage path
  return $ case dImg of
    Left msg  -> Left msg
    Right img -> Right $ DImage w h (ImageRef path)
      where
        w = dynamicMap imageWidth img
        h = dynamicMap imageHeight img

-- | Make an "unchecked" image reference; have to specify a
--   width and height. Unless the aspect ratio of the external
--   image is the w :: h, then the image will be distorted.
uncheckedImageRef :: Int -> Int -> FilePath -> DImage External
uncheckedImageRef w h path = DImage w h (ImageRef path)

-- | Convert an external image to an embedded one.
externalToEmbedded :: DImage External -> IO (Either String (DImage Embedded))
externalToEmbedded (DImage _ _ (ImageRef path)) = loadImageEmb path

-- | Convert an embedded image to an external one, saving the image to
--   the path. This can be useful for backends that don't support
--   embedded images. The type of image used depends on the extension of
--   the output file.
embeddedToExternal :: FilePath -> DImage Embedded -> IO (Either String (DImage External))
embeddedToExternal path (DImage w h (ImageRaster dyn)) =
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

    ('.':ext) -> pure $ Left ("Unknown file extension: " ++ ext)

    _       -> pure $ Left "No file extension given"

  where exImg = pure . Right $ DImage w h (ImageRef path)

