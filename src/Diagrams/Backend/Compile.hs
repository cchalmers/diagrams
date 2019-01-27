{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE TypeApplications      #-}
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

import           Control.Lens                 hiding (transform)
import           Data.Colour                  (Colour)
import qualified Data.Foldable                as F
import qualified Data.Monoid                  as M
import           Data.Monoid.Coproduct.Strict
import           Data.Sequence                (Seq)
import           Data.Typeable

import           Codec.Picture.Types          (DynamicImage)
import           Geometry.Envelope            (size)
import           Geometry.Path                (Path)
import           Geometry.Space
import           Geometry.ThreeD.Shapes
import           Geometry.ThreeD.Types
import           Geometry.Transform

import           Diagrams.Types
import           Diagrams.Types.Tree          (foldDUAL)

import           Diagrams.Attributes
import           Diagrams.ThreeD.Light        (ParallelLight (..),
                                               PointLight (..))
import           Diagrams.TwoD.Attributes
import           Diagrams.TwoD.Image          (DImage (..), Embedded, External)
import           Diagrams.TwoD.Text           (Text)
import           Linear                       (Additive, V2 (..))

import           Data.Coerce

-- Typeable1 is a depreciated synonym in ghc > 707
#if __GLASGOW_HASKELL__ >= 707
#define Typeable1 Typeable
#endif

foldDiaWithScales
  :: forall v n m r.
     (HasLinearMap v, OrderedField n, M.Monoid r)
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
  :: forall v n m r.
    (HasLinearMap v, OrderedField n, Monoid r)
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

-- | Monoid under '*>'
newtype Ap f a = Ap (f a)
instance (Applicative f, a ~ ()) => Semigroup (Ap f a) where
  (<>) = coerce ((*>) @f @() @())
instance (Applicative f, a ~ ()) => Monoid (Ap f a) where
  mempty  = Ap (pure ())
  mappend = (<>)

-- | Applicative version of 'foldDia' with @mempty = pure ()@ and @mappend =
--   (*>)@
foldDiaA
  :: forall v n m f.
     (HasLinearMap v, OrderedField n, Applicative f)
  => (Transformation v n -> Attributes -> Prim v n -> f ()) -- ^ Fold a prim
  -> (Annotation v n -> f () -> f ())   -- ^ Apply an annotation
  -> Transformation v n           -- ^ final transform for diagram
  -> QDiagram v n m               -- ^ diagram to fold
  -> f ()
foldDiaA = coerce (foldDia @v @n @m @(Ap f ()))
{-# INLINE foldDiaA #-}

-- | Applicative version of 'foldDiaWithScales' with @mempty = pure ()@
--   and @mappend = (*>)@
foldDiaWithScalesA
  :: forall v n m f.
     (HasLinearMap v, OrderedField n, Applicative f)
  => (Transformation v n -> Attributes -> Prim v n -> f ())
  -> (Annotation v n -> f () -> f ())
  -> n -- 'global' to 'output' scale factor
  -> n -- 'normalised' to 'output' scale factor
  -> QDiagram v n m -- ^ diagram to fold
  -> f ()
foldDiaWithScalesA = coerce (foldDiaWithScales @v @n @m @(Ap f ()))
{-# INLINE foldDiaWithScalesA #-}


-- Standard prisms -----------------------------------------------------

-- | Prism onto to a path.
_Path :: (Typeable v, Additive v, Typeable n, Num n) => Prism' (Prim v n) (Path v n)
_Path = _Prim

-- | Prism onto to an unboxed path
-- _UPath :: (Typeable v, Additive v, Typeable n, Num n) => Prism' (Prim v n) (UPath v n)
-- _UPath = _Prim

-- | Prism onto to a text.
_Text :: (Typeable n, Num n) => Prism' (Prim V2 n) (Text n)
_Text = _Prim

-- | Prism onto to an embedded image.
_ExternalImage :: Prism' (Prim V2 Double) (DImage External)
_ExternalImage = _Prim

-- | Prism onto to an embedded image.
_EmbeddedImage :: Prism' (Prim V2 Double) (DImage Embedded)
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

_PointLight :: Prism' (Prim V3 Double) PointLight
_PointLight = _Prim

_ParallelLight :: Prism' (Prim V3 Double) ParallelLight
_ParallelLight = _Prim

_CSG :: Prism' (Prim V3 Double) (CSG Double)
_CSG = _Prim

-- Patterns ------------------------------------------------------------

pattern Prim_ :: (InSpace v n a, Typeable a) => a -> Prim v n
pattern Prim_ a <- (preview _Prim -> Just a) where
  Prim_ a = Prim a

pattern Cube_ :: (Typeable n, Num n) => Prim V3 n
pattern Cube_ = Prim_ Cube

pattern Frustum_ :: (Typeable n, Num n) => n -> n -> Prim V3 n
pattern Frustum_ a b = (Prim_ (Frustum a b))

pattern Sphere_ :: (Typeable n, Num n) => Prim V3 n
pattern Sphere_ = Prim_ Sphere

pattern CSG_ :: CSG Double -> Prim V3 Double
pattern CSG_ a = Prim_ a

pattern PointLight_ :: P3 Double -> Colour Double -> Prim V3 Double
pattern PointLight_ p c = (Prim_ (PointLight p c))

pattern ParallelLight_ :: V3 Double -> Colour Double -> Prim V3 Double
pattern ParallelLight_ p c = (Prim_ (ParallelLight p c))

pattern Path_ :: (Typeable v, Additive v, Num n, Typeable n)
              => Path v n -> Prim v n
pattern Path_ p = Prim_ p

pattern Text_ :: (Typeable n, Num n) => Text n -> Prim V2 n
pattern Text_ p = Prim_ p

pattern EmbeddedImage_ :: DynamicImage -> Prim V2 Double
pattern EmbeddedImage_ dyn
  <- (preview _EmbeddedImage -> Just (ImageEmbedded dyn)) where
  EmbeddedImage_ img = Prim (ImageEmbedded img)

pattern ExternalImage_ :: Int -> Int -> FilePath -> Prim V2 Double
pattern ExternalImage_ w h path
  <- (preview _ExternalImage -> Just (ImageExternal (V2 w h) path)) where
  ExternalImage_ w h path = Prim (ImageExternal (V2 w h) path)

-- Annotations ---------------------------------------------------------

pattern GroupOpacity_ :: Double -> Annotation V2 Double
pattern GroupOpacity_ a <- (getAnnot _GroupOpacity -> Just a) where
  GroupOpacity_ = mkAnnot _GroupOpacity

pattern Clip_ :: Seq (Path V2 Double) -> Annotation V2 Double
pattern Clip_ a <- (getAnnot _Clip -> Just a) where
  Clip_ = mkAnnot _Clip

pattern Shading_ :: Diagram V2 -> Annotation V2 Double
pattern Shading_ a <- (getAnnot _Shading -> Just a) where
  Shading_ = mkAnnot _Shading

pattern HRef_ :: String -> Annotation V2 Double
pattern HRef_ a <- (getAnnot _HRef -> Just a) where
  HRef_ = mkAnnot _HRef

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
