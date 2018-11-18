{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Attributes
-- Copyright   :  (c) 2014 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Diagrams may have /attributes/ which affect the way they are
-- rendered.  This module defines some common attributes relevant in
-- 3D; particular backends may also define more backend-specific
-- attributes.
--
-- Every attribute type must have a /semigroup/ structure, that is, an
-- associative binary operation for combining two attributes into one.
-- Unless otherwise noted, all the attributes defined here use the
-- 'Last' structure, that is, combining two attributes simply keeps
-- the second one and throws away the first.  This means that child
-- attributes always override parent attributes.
--
-----------------------------------------------------------------------------

module Diagrams.ThreeD.Attributes
  (
    -- * Surface colour
    SurfaceColor
  , _SurfaceColor
  , sc
  , _sc

    -- * Diffuse
  , Diffuse
  , _Diffuse
  , diffuse
  , _diffuse

    -- * Highlight
  , Ambient
  , _Ambient
  , ambient
  , _ambient

    -- * Highlight
  , Highlight
  , _Highlight
  , highlight
  , _highlight

    -- ** Specular
  , Specular (..)
  , specularIntensity
  , specularSize
  ) where

import           Control.Lens
import           Data.Semigroup
import           Data.Typeable

import           Data.Colour

import           Diagrams.Types.Style

-- Surface colour ------------------------------------------------------

-- | @SurfaceColor@ is the inherent pigment of an object, assumed to
--   be opaque.
newtype SurfaceColor = SurfaceColor (Last (Colour Double))
  deriving (Typeable, Semigroup, Show)

instance AttributeClass SurfaceColor where
  type AttrType SurfaceColor = 'IAttr

_SurfaceColor :: Iso' SurfaceColor (Colour Double)
_SurfaceColor = coerced

-- | Set the surface color.
sc :: ApplyStyle d => Colour Double -> d -> d
sc = applyAttr _SurfaceColor

-- | Lens onto the surface colour of a style.
_sc :: HasStyle a => Lens' a (Maybe (Colour Double))
_sc = style . atAttr _SurfaceColor

-- Diffuse -------------------------------------------------------------

-- | @Diffuse@ is the fraction of incident light reflected diffusely,
-- that is, in all directions.  The actual light reflected is the
-- product of this value, the incident light, and the @SurfaceColor@
-- Attribute.  For physical reasonableness, @Diffuse@ should have a
-- value between 0 and 1; this is not checked.
newtype Diffuse = Diffuse (Last Double)
  deriving (Typeable, Semigroup, Show)

instance AttributeClass Diffuse where
  type AttrType Diffuse = 'IAttr

-- | Isomorphism between 'Diffuse' and 'Double'
_Diffuse :: Iso' Diffuse Double
_Diffuse = coerced

-- | Set the diffuse reflectance.
diffuse :: ApplyStyle d => Double -> d -> d
diffuse = applyAttr _Diffuse

-- | Lens onto the possible diffuse reflectance in a style.
_diffuse :: Lens' (Style v Double) (Maybe Double)
_diffuse = style . atAttr _Diffuse

-- Ambient -------------------------------------------------------------

-- | @Ambient@ is an ad-hoc representation of indirect lighting.  The
-- product of @Ambient@ and @SurfaceColor@ is added to the light
-- leaving an object due to diffuse and specular terms.  @Ambient@ can
-- be set per-object, and can be loosely thought of as the product of
-- indirect lighting incident on that object and the diffuse
-- reflectance.
newtype Ambient = Ambient (Last Double)
  deriving (Typeable, Semigroup, Show)

instance AttributeClass Ambient where
  type AttrType Ambient = 'IAttr

_Ambient :: Iso' Ambient Double
_Ambient = coerced

-- | Set the emittance due to ambient light.
ambient :: ApplyStyle d => Double -> d -> d
ambient = applyAttr _Ambient

-- | Lens onto the possible ambience in a style.
_ambient :: HasStyle a => Lens' a (Maybe Double)
_ambient = style . atAttr _Ambient

-- Specular ------------------------------------------------------------

-- | A specular highlight has two terms, the intensity, between 0 and
-- 1, and the size.  The highlight size is assumed to be the exponent
-- in a Phong shading model (though Backends are free to use a
-- different shading model).  In this model, reasonable values are
-- between 1 and 50 or so, with higher values for shinier objects.
-- Physically, the intensity and the value of @Diffuse@ must add up to
-- less than 1; this is not enforced.
data Specular = Specular
  { _specularIntensity :: Double
  , _specularSize      :: Double
  } deriving Show

makeLenses ''Specular

-- Highlight -----------------------------------------------------------

newtype Highlight = Highlight (Last Specular)
  deriving (Typeable, Semigroup, Show)

instance AttributeClass Highlight where
  type AttrType Highlight = 'IAttr
  -- should this be a transformable attritute that transforms the
  -- specularSize?

_Highlight :: Iso' Highlight Specular
_Highlight = coerced

-- | Set the specular highlight.
highlight :: ApplyStyle a => Specular -> a -> a
highlight = applyAttr _Highlight

-- | Lens onto the possible specular highlight in a style
_highlight :: HasStyle a => Lens' a (Maybe Specular)
_highlight = style . atAttr _Highlight

-- | Traversal over the highlight intensity of a style. If the style has
--   no 'Specular', setting this will do nothing.
-- highlightIntensity :: Traversal' (Style v n) Double
-- highlightIntensity = _highlight . _Just . specularSize

-- | Traversal over the highlight size in a style. If the style has no
--   'Specular', setting this will do nothing.
-- highlightSize :: Traversal' (Style v n) Double
-- highlightSize = _highlight . _Just . specularSize

