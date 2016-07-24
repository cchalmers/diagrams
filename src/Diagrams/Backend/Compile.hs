{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeOperators         #-}

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
import qualified Data.Foldable             as F
import           Data.Monoid.Coproduct
import qualified Data.Monoid as            M
import           Data.Tree.DUAL.Label      (foldDUAL)
import           Data.Typeable

import           Geometry.Envelope    (size)
import           Geometry.Transform
import           Geometry.Space
import           Geometry.Path (Path)
import           Geometry.Path.Unboxed (UPath)

import           Diagrams.Types

import Diagrams.TwoD.Image (DImage, Embedded, External)
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
  :: (HasLinearMap v, OrderedField n, M.Monoid r)
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
_EmbeddedImage :: (Typeable n, Num n) => Prism' (Prim V2 n) (DImage n Embedded)
_EmbeddedImage = _Prim

-- | Prism onto to an external image.
_ExternalImage :: (Typeable n, Num n) => Prism' (Prim V2 n) (DImage n External)
_ExternalImage = _Prim

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
