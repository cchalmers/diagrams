{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ViewPatterns          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Compile
-- Copyright   :  (c) 2013-2016 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module provides tools for compiling @QDiagrams@ into a more
-- convenient and optimized tree form, suitable for use by backends.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Compile
  -- ( -- * Tools for backends
  --   foldDia
  -- , foldDia'

  --   -- * Backend API

  -- , renderDia
  -- , renderDiaT
  -- )
  where

import           Control.Lens              hiding (transform)
import           Control.Lens.Extras       (is)
import qualified Data.Foldable             as F
import           Data.Monoid.Coproduct
import qualified Data.Monoid as            M
import           Data.Typeable
import Data.Colour (Colour)

import           Geometry.Envelope    (size)
import           Geometry.Transform
import           Geometry.Space
import           Geometry.Path (Path)
import           Geometry.Path.Unboxed (UPath)
import           Geometry.ThreeD.Shapes
import           Geometry.ThreeD.Types
import           Codec.Picture.Types  (DynamicImage, dynamicMap, imageWidth, imageHeight)

import           Diagrams.Types
import           Diagrams.Types.Tree (foldDUAL)

import Diagrams.TwoD.Image (DImage(..), ImageData(..), Embedded, External)
import Diagrams.ThreeD.Light (PointLight (..))
import Diagrams.TwoD.Text (Text)

-- Typeable1 is a depreciated synonym in ghc > 707
#if __GLASGOW_HASKELL__ >= 707
#define Typeable1 Typeable
#endif

foldDiaWithScales
  :: (HasLinearMap v, M.Monoid r)
  => (Transformation v Double -> Attributes -> Prim -> r)
  -> (Annotation v -> r -> r)
  -> Double -- 'global' to 'output' scale factor
  -> Double -- 'normalised' to 'output' scale factor
  -> QDiagram v m -- ^ diagram to fold
  -> r
foldDiaWithScales primF aF g n (QD dual) = foldDUAL lF aF dual
  where
    lF d = \case
      PrimLeaf p    ->
        let (tr, sty) = untangle d
        in  primF tr (getAttributes g n sty) p
      DelayedLeaf f ->
        let (QD dia) = f d g n
        in  foldDUAL lF aF dia
{-# INLINE foldDiaWithScales #-}

-- | Simple way to fold a diagram into a monoidal result.
foldDia
  :: (HasLinearMap v, Monoid r)
  => (Transformation v Double -> Attributes -> Prim -> r) -- ^ Fold a prim
  -> (Annotation v -> r -> r)   -- ^ Apply an annotation
  -> Transformation v Double    -- ^ final transform for diagram
  -> QDiagram v m               -- ^ diagram to fold
  -> r
foldDia primF annF t d = foldDiaWithScales primF annF g n d
  where
    g = avgScale t
    n = normalizedFactor (size d)
{-# INLINE foldDia #-}

-- Standard prisms -----------------------------------------------------

-- | Prism onto to a path.
_Path :: Typeable v => Prism' Prim (Path v Double)
_Path = _Prim

-- | Prism onto to an unboxed path
_UPath :: Typeable v => Prism' Prim (UPath v Double)
_UPath = _Prim

-- | Prism onto to a text.
_Text :: Prism' Prim Text
_Text = _Prim

-- | Prism onto to an embedded image.
_ExternalImage :: Prism' Prim (DImage External)
_ExternalImage = _Prim

-- | Prism onto to an embedded image.
_EmbeddedImage :: Prism' Prim (DImage Embedded)
_EmbeddedImage = _Prim

------------------------------------------------------------------------
-- 3D
------------------------------------------------------------------------

-- Prisms --------------------------------------------------------------

-- | Prism onto a cube prim.
_Cube :: Prism' Prim (Cube Double)
_Cube = _Prim

-- | Prism onto a cube prim.
_Frustum :: Prism' Prim (Frustum Double)
_Frustum = _Prim

-- | Prism onto a cube prim.
_Sphere :: Prism' Prim (Sphere Double)
_Sphere = _Prim

_PointLight :: Prism' Prim PointLight
_PointLight = _Prim

-- Patterns ------------------------------------------------------------

pattern Cube_ :: Prim
pattern Cube_ <- (is _Cube -> True) where
  Cube_ = Prim (Cube :: Cube Double)

pattern Frustum_ :: Double -> Double -> Prim
pattern Frustum_ a b <- (preview _Frustum -> Just (Frustum a b)) where
  Frustum_ a b = Prim (Frustum a b)

pattern Sphere_ :: Prim
pattern Sphere_ <- (is _Sphere -> True) where
  Sphere_ = Prim (Sphere :: Sphere Double)

pattern PointLight_ :: P3 Double -> Colour Double -> Prim
pattern PointLight_ p c <- (preview _PointLight -> Just (PointLight p c)) where
  PointLight_ p c = Prim (PointLight p c)

pattern Path_ :: Typeable v => Path v Double -> Prim
pattern Path_ p <- (preview _Path -> Just p) where
  Path_ p = Prim p

pattern UPath_ :: Typeable v => UPath v Double -> Prim
pattern UPath_ p <- (preview _UPath -> Just p) where
  UPath_ p = Prim p

pattern Text_ :: Typeable n => Text -> Prim
pattern Text_ p <- (preview _Text -> Just p) where
  Text_ p = Prim p

pattern EmbeddedImage_ :: DynamicImage -> Prim
pattern EmbeddedImage_ dyn
  <- (preview _EmbeddedImage -> Just (DImage _ _ (ImageRaster dyn))) where
  EmbeddedImage_ img =
    let w = dynamicMap imageWidth img
        h = dynamicMap imageHeight img
    in  Prim (DImage w h (ImageRaster img))

pattern ExternalImage_ :: Int -> Int -> FilePath -> Prim
pattern ExternalImage_ w h path
  <- (preview _ExternalImage -> Just (DImage w h (ImageRef path))) where
  ExternalImage_ w h path = Prim (DImage w h (ImageRef path))

-- | Get the normalized scale factor from a vector. For the
--   'normalizedFactor' of a diagram use this with the 'size' of the
--   diagram.
--
--   Note: The 'global' factor is the 'avgScale' of the output
--   transform.
normalizedFactor :: (F.Foldable v, Floating n) => v n -> n
normalizedFactor v = F.product v ** (1 / fromIntegral (lengthOf folded v))

-- | Render a diagram.
-- renderDia
--   :: (Backend b v n , HasLinearMap v, Metric v, Typeable1 v,
--       Typeable n, OrderedField n, Monoid' m)
--   => Options b v n -> QDiagram v n m -> Result b v n
-- renderDia b opts d = snd (renderDiaT b opts d)
