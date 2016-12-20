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
import Linear (Additive, V2)

-- Typeable1 is a depreciated synonym in ghc > 707
#if __GLASGOW_HASKELL__ >= 707
#define Typeable1 Typeable
#endif

foldDiaWithScales
  :: (HasLinearMap v, OrderedField n, M.Monoid r)
  => (Transformation v n -> Attributes -> Prim v n -> r)
  -> (Annotation v n -> r -> r)
  -> n -- 'global' to 'output' scale factor
  -> n -- 'normalised' to 'output' scale factor
  -> QDiagram v n m -- ^ diagram to fold
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
  :: (HasLinearMap v, OrderedField n, Monoid r)
  => (Transformation v n -> Attributes -> Prim v n -> r) -- ^ Fold a prim
  -> (Annotation v n -> r -> r)   -- ^ Apply an annotation
  -> Transformation v n           -- ^ final transform for diagram
  -> QDiagram v n m               -- ^ diagram to fold
  -> r
foldDia primF annF t d = foldDiaWithScales primF annF g n d
  where
    g = avgScale t
    n = normalizedFactor (size d)
{-# INLINE foldDia #-}

-- Standard prisms -----------------------------------------------------

-- | Prism onto to a path.
_Path :: (Typeable v, Additive v, Typeable n, Num n) => Prism' (Prim v n) (Path v n)
_Path = _Prim

-- | Prism onto to an unboxed path
_UPath :: (Typeable v, Additive v, Typeable n, Num n) => Prism' (Prim v n) (UPath v n)
_UPath = _Prim

-- | Prism onto to a text.
_Text :: (Typeable n, Num n) => Prism' (Prim V2 n) (Text n)
_Text = _Prim

-- | Prism onto to an embedded image.
_ExternalImage :: (Typeable n, Num n) => Prism' (Prim V2 n) (DImage n External)
_ExternalImage = _Prim

-- | Prism onto to an embedded image.
_EmbeddedImage :: (Typeable n, Num n) => Prism' (Prim V2 n) (DImage n Embedded)
_EmbeddedImage = _Prim

------------------------------------------------------------------------
-- 3D
------------------------------------------------------------------------

-- Prisms --------------------------------------------------------------

-- | Prism onto a cube prim.
_Cube :: (Typeable n, Num n) => Prism' (Prim V3 n) (Cube n)
_Cube = _Prim

-- | Prism onto a cube prim.
_Frustum :: (Typeable n, Num n) => Prism' (Prim V3 n) (Frustum n)
_Frustum = _Prim

-- | Prism onto a cube prim.
_Sphere :: (Typeable n, Num n) => Prism' (Prim V3 n) (Sphere n)
_Sphere = _Prim

_PointLight :: (Typeable n, Num n) => Prism' (Prim V3 n) (PointLight n)
_PointLight = _Prim

-- Patterns ------------------------------------------------------------

pattern Cube_ :: (Typeable n, Num n) => Prim V3 n
pattern Cube_ <- (is _Cube -> True) where
  Cube_ = Prim Cube

pattern Frustum_ :: (Typeable n, Num n) => n -> n -> Prim V3 n
pattern Frustum_ a b <- (preview _Frustum -> Just (Frustum a b)) where
  Frustum_ a b = Prim (Frustum a b)

pattern Sphere_ :: (Typeable n, Num n) => Prim V3 n
pattern Sphere_ <- (is _Sphere -> True) where
  Sphere_ = Prim Sphere

pattern PointLight_ :: (Typeable n, Num n) => P3 n -> Colour Double -> Prim V3 n
pattern PointLight_ p c <- (preview _PointLight -> Just (PointLight p c)) where
  PointLight_ p c = Prim (PointLight p c)

pattern Path_ :: (Typeable v, Additive v, Num n, Typeable n)
              => Path v n -> Prim v n
pattern Path_ p <- (preview _Path -> Just p) where
  Path_ p = Prim p

pattern UPath_ :: (Typeable v, Additive v, Typeable n, Num n)
               => UPath v n -> Prim v n
pattern UPath_ p <- (preview _UPath -> Just p) where
  UPath_ p = Prim p

pattern Text_ :: (Typeable n, Num n) => Text n -> Prim V2 n
pattern Text_ p <- (preview _Text -> Just p) where
  Text_ p = Prim p

pattern EmbeddedImage_ :: (Typeable n, Num n) => DynamicImage -> Prim V2 n
pattern EmbeddedImage_ dyn
  <- (preview _EmbeddedImage -> Just (DImage _ _ (ImageRaster dyn))) where
  EmbeddedImage_ img =
    let w = dynamicMap imageWidth img
        h = dynamicMap imageHeight img
    in  Prim (DImage w h (ImageRaster img))

pattern ExternalImage_ :: (Typeable n, Num n) => Int -> Int -> FilePath -> Prim V2 n
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
