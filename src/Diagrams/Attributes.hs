{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Attributes
-- Copyright   :  (c) 2011-2015 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Diagrams may have /attributes/ which affect the way they are
-- rendered.  This module defines some common attributes; particular
-- backends may also define more backend-specific attributes.
--
-- Every attribute type must have a /semigroup/ structure, that is, an
-- associative binary operation for combining two attributes into one.
-- Unless otherwise noted, all the attributes defined here use the
-- 'Last' structure, that is, combining two attributes simply keeps
-- the second one and throws away the first.  This means that child
-- attributes always override parent attributes.
--
-----------------------------------------------------------------------------

module Diagrams.Attributes (
    -- ** Standard measures
    ultraThin, veryThin, thin, medium, thick, veryThick, ultraThick, none
  , tiny, verySmall, small, normal, large, veryLarge, huge

    -- ** Line width
  , LineWidth, _LineWidth
  , lineWidth, _lineWidth
  , _lw, lw, lwN, lwO, lwL, lwG

    -- ** Dashing
  , Dashing(..), _Dashing
  , dashing, _dashing
  , dashingN, dashingO, dashingL, dashingG

  -- * Color
  -- $color

  , Color(..), SomeColor(..)
  , _SomeColor, someToAlpha

  -- ** Opacity
  , Opacity, _Opacity
  , opacity, _opacity

  , FillOpacity, _FillOpacity
  , fillOpacity, _fillOpacity

  , StrokeOpacity, _StrokeOpacity
  , strokeOpacity, _strokeOpacity

  -- ** Converting colors
  , colorToSRGBA, colorToRGBA

  -- * Line stuff
  -- ** Cap style
  , LineCap(..), _LineCap
  , lineCap, _lineCap

  -- ** Join style
  , LineJoin(..), _LineJoin
  , lineJoin, _lineJoin

  -- ** Miter limit
  , LineMiterLimit, _LineMiterLimit
  , lineMiterLimit, _lineMiterLimit

  -- * Annotations

  -- ** GroupOpacity
  , GroupOpacity
  , _GroupOpacity
  , groupOpacity

  -- ** Hyperlink reference
  , HRef
  , _HRef
  , href

  ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Lens         hiding (none, over)
import           Data.Coerce
import           Data.Colour
import           Data.Colour.RGBSpace (RGB (..))
import           Data.Colour.SRGB     (toSRGB)
import           Data.Default.Class
import           Data.Distributive
import           Data.Semigroup
import           Data.Typeable


import           Geometry.Space
import           Geometry.TwoD.Offset (LineCap (..), LineJoin (..))

import           Diagrams.Types

------------------------------------------------------------------------
-- Standard measures
------------------------------------------------------------------------

none, ultraThin, veryThin, thin, medium, thick, veryThick, ultraThick,
  tiny, verySmall, small, normal, large, veryLarge, huge
  :: OrderedField n => Measure n
none       = output 0
ultraThin  = normalized 0.0005 `atLeast` output 0.5
veryThin   = normalized 0.001  `atLeast` output 0.5
thin       = normalized 0.002  `atLeast` output 0.5
medium     = normalized 0.004  `atLeast` output 0.5
thick      = normalized 0.0075 `atLeast` output 0.5
veryThick  = normalized 0.01   `atLeast` output 0.5
ultraThick = normalized 0.02   `atLeast` output 0.5

tiny      = normalized 0.01
verySmall = normalized 0.015
small     = normalized 0.023
normal    = normalized 0.035
large     = normalized 0.05
veryLarge = normalized 0.07
huge      = normalized 0.10

------------------------------------------------------------------------
-- Line width
------------------------------------------------------------------------

-- | Line widths specified on child nodes always override line widths
--   specified at parent nodes. The 'Default' line width is 'medium'.
newtype LineWidth n = LineWidth (Last n)
  deriving (Typeable, Semigroup)

_LineWidth :: Iso' (LineWidth n) n
_LineWidth = iso coerce coerce

instance Typeable n => AttributeClass (LineWidth n) where
  type AttrType (LineWidth n) = 'MAttr

-- | Set the line (stroke) width.
lineWidth :: (N a ~ n, ApplyStyle a, Typeable n) => Measure n -> a -> a
lineWidth = applyAttr _LineWidth

instance Num n => Default (LineWidth n) where
  def = review _LineWidth 1

-- | Default for 'lineWidth'.
lw :: (N a ~ n, ApplyStyle a, Typeable n) => Measure n -> a -> a
lw = lineWidth

-- | A convenient synonym for @'lineWidth' ('global' w)@.
lwG :: (N a ~ n, ApplyStyle a, Typeable n, Num n) => n -> a -> a
lwG = lw . global

-- | A convenient synonym for @'lineWidth' ('normalized' w)@.
lwN :: (N a ~ n, ApplyStyle a, Typeable n, Num n) => n -> a -> a
lwN = lw . normalized

-- | A convenient synonym for @'lineWidth' ('output' w)@.
lwO :: (N a ~ n, ApplyStyle a, Typeable n) => n -> a -> a
lwO = lw . output

-- | A convenient sysnonym for @'lineWidth' ('local' w)@.
lwL :: (N a ~ n, ApplyStyle a, Typeable n, Num n) => n -> a -> a
lwL = lw . local

-- | Lens onto a measured line width in a style.
_lineWidth, _lw :: (Typeable (N a), HasStyle a) => Lens' a (Maybe (Measure (N a)))
_lineWidth = style . atAttr _LineWidth
_lw = _lineWidth

------------------------------------------------------------------------
-- Dashing
------------------------------------------------------------------------

-- | Create lines that are dashing... er, dashed.
data Dashing n = Dashing [n] n
  deriving (Functor, Typeable, Eq)

-- | 'Dashing' isomorphism.
_Dashing :: Equality' (Dashing n) (Dashing n)
_Dashing = id

instance Semigroup (Dashing n) where
  _ <> b = b

instance Typeable n => AttributeClass (Dashing n) where
  type AttrType (Dashing n) = 'MAttr

-- | Set the line dashing style.
dashing :: (N a ~ n, ApplyStyle a, Typeable n)
        => [Measure n]  -- ^ A list specifying alternate lengths of on
                        --   and off portions of the stroke.  The empty
                        --   list indicates no dashing.
        -> Measure n    -- ^ An offset into the dash pattern at which the
                        --   stroke should start.
        -> a -> a
dashing ds offs = applyAttr _Dashing . distribute $ Dashing ds offs

-- | A convenient synonym for 'dashing (global w)'.
dashingG :: (N a ~ n, ApplyStyle a, Typeable n, Num n) => [n] -> n -> a -> a
dashingG w v = dashing (map global w) (global v)

-- | A convenient synonym for 'dashing (normalized w)'.
dashingN :: (N a ~ n, ApplyStyle a, Typeable n, Num n) => [n] -> n -> a -> a
dashingN w v = dashing (map normalized w) (normalized v)

-- | A convenient synonym for 'dashing (output w)'.
dashingO :: (N a ~ n, ApplyStyle a, Typeable n) => [n] -> n -> a -> a
dashingO w v = dashing (map output w) (output v)

-- | A convenient sysnonym for 'dashing (local w)'.
dashingL :: (N a ~ n, ApplyStyle a, Typeable n, Num n) => [n] -> n -> a -> a
dashingL w v = dashing (map local w) (local v)

-- | Lens onto a measured dashing attribute in a style. Note you can
--   convert a @'Dashing' ('Measure' n)@ to @'Measured' n ('Dashing' n)@
--   using 'distribute'.
_dashing :: (n ~ N a, Typeable n)
         => HasStyle a => Lens' a (Maybe (Measured n (Dashing n)))
_dashing = style . atAttr _Dashing

------------------------------------------------------------------------
-- Color
------------------------------------------------------------------------

-- $color
-- Diagrams outsources all things color-related to Russell O\'Connor\'s
-- very nice colour package
-- (<http://hackage.haskell.org/package/colour>).  For starters, it
-- provides a large collection of standard color names.  However, it
-- also provides a rich set of combinators for combining and
-- manipulating colors; see its documentation for more information.

-- | The 'Color' type class encompasses color representations which
--   can be used by the Diagrams library.  Instances are provided for
--   both the 'Data.Colour.Colour' and 'Data.Colour.AlphaColour' types
--   from the "Data.Colour" library.
class Color c where
  -- | Convert a color to its standard representation, AlphaColour.
  toAlphaColour :: c -> AlphaColour Double

  -- | Convert from an AlphaColour Double.  Note that this direction
  --   may lose some information. For example, the instance for
  --   'Colour' drops the alpha channel.
  fromAlphaColour :: AlphaColour Double -> c

-- | An existential wrapper for instances of the 'Color' class.
data SomeColor = forall c. Color c => SomeColor c
  deriving Typeable

instance Show SomeColor where
  showsPrec d (colorToSRGBA -> (r,g,b,a)) =
    showParen (d > 10) $ showString "SomeColor " .
      if a == 0
        then showString "transparent"
        else showString "(sRGB " . showsPrec 11 r . showChar ' '
                                 . showsPrec 11 g . showChar ' '
                                 . showsPrec 11 b .
                        (if a /= 1
                           then showString " `withOpacity` " . showsPrec 11 a
                           else id) . showChar ')'

-- | Isomorphism between 'SomeColor' and 'AlphaColour' 'Double'.
_SomeColor :: Iso' SomeColor (AlphaColour Double)
_SomeColor = iso toAlphaColour fromAlphaColour

someToAlpha :: SomeColor -> AlphaColour Double
someToAlpha (SomeColor c) = toAlphaColour c

instance a ~ Double => Color (Colour a) where
  toAlphaColour   = opaque
  fromAlphaColour = (`over` black)

instance a ~ Double => Color (AlphaColour a) where
  toAlphaColour   = id
  fromAlphaColour = id

instance Color SomeColor where
  toAlphaColour (SomeColor c) = toAlphaColour c
  fromAlphaColour             = SomeColor

-- | Convert to sRGBA.
colorToSRGBA, colorToRGBA :: Color c => c -> (Double, Double, Double, Double)
colorToSRGBA col = (r, g, b, a)
  where
    c' = toAlphaColour col
    c = alphaToColour c'
    a = alphaChannel c'
    RGB r g b = toSRGB c

colorToRGBA = colorToSRGBA
{-# DEPRECATED colorToRGBA "Renamed to colorToSRGBA." #-}

alphaToColour :: (Floating a, Ord a) => AlphaColour a -> Colour a
alphaToColour ac | alphaChannel ac == 0 = ac `over` black
                 | otherwise = darken (recip (alphaChannel ac)) (ac `over` black)

------------------------------------------------------------------------
-- Opacity
------------------------------------------------------------------------

-- | Although the individual colors in a diagram can have
--   transparency, the opacity/transparency of a diagram as a whole
--   can be specified with the @Opacity@ attribute.  The opacity is a
--   value between 1 (completely opaque, the default) and 0
--   (completely transparent).  Opacity is multiplicative, that is,
--   @'opacity' o1 . 'opacity' o2 === 'opacity' (o1 * o2)@.  In other
--   words, for example, @opacity 0.8@ means \"decrease this diagram's
--   opacity to 80% of its previous opacity\".
newtype Opacity = Opacity (Product Double)
  deriving (Typeable, Semigroup)

instance AttributeClass Opacity where
  type AttrType Opacity = 'IAttr

_Opacity :: Iso' Opacity Double
_Opacity = coerced

instance Default Opacity where
  def = Opacity 1

-- | Multiply the opacity (see 'Opacity') by the given value.  For
--   example, @opacity 0.8@ means \"decrease this diagram's opacity to
--   80% of its previous opacity\".
opacity :: ApplyStyle a => Double -> a -> a
opacity = applyAttr _Opacity

-- | Lens onto the opacity in a style.
_opacity :: HasStyle a => Lens' a (Maybe Double)
_opacity = style . atAttr _Opacity

-- fill opacity --------------------------------------------------------

-- | Like 'Opacity', but set the opacity only for fills (as opposed to strokes).
--   As with 'Opacity', the fill opacity is a value between 1
--   (completely opaque, the default) and 0 (completely transparent),
--   and is multiplicative.
newtype FillOpacity = FillOpacity (Product Double)
  deriving (Typeable, Semigroup)

instance AttributeClass FillOpacity where
  type AttrType FillOpacity = 'IAttr

_FillOpacity :: Iso' FillOpacity Double
_FillOpacity = coerced

instance Default FillOpacity where
  def = FillOpacity 1

-- | Multiply the fill opacity (see 'FillOpacity') by the given value.  For
--   example, @fillOpacity 0.8@ means \"decrease this diagram's fill opacity to
--   80% of its previous value\".
fillOpacity :: ApplyStyle a => Double -> a -> a
fillOpacity = applyAttr _FillOpacity

-- | Lens onto the fill opacity in a style.
_fillOpacity :: HasStyle a => Lens' a Double
_fillOpacity = style . atAttr _FillOpacity . non 1

-- stroke opacity --------------------------------------------------------

-- | Like 'Opacity', but set the opacity only for strokes (as opposed to fills).
--   As with 'Opacity', the fill opacity is a value between 1
--   (completely opaque, the default) and 0 (completely transparent),
--   and is multiplicative.
newtype StrokeOpacity = StrokeOpacity (Product Double)
  deriving (Typeable, Semigroup)

instance AttributeClass StrokeOpacity where
  type AttrType StrokeOpacity = 'IAttr

_StrokeOpacity :: Iso' StrokeOpacity Double
_StrokeOpacity = coerced

instance Default StrokeOpacity where
  def = StrokeOpacity 1

-- | Multiply the stroke opacity (see 'StrokeOpacity') by the given value.  For
--   example, @strokeOpacity 0.8@ means \"decrease this diagram's
--   stroke opacity to 80% of its previous value\".
strokeOpacity :: ApplyStyle a => Double -> a -> a
strokeOpacity = applyAttr _StrokeOpacity

-- | Lens onto the stroke opacity in a style.
_strokeOpacity :: HasStyle a => Lens' a Double
_strokeOpacity = style . atAttr _StrokeOpacity . non 1

------------------------------------------------------------------------
-- Line stuff
------------------------------------------------------------------------

-- line cap ------------------------------------------------------------

-- -- 'LineCap' is now defined in "Geometry.TwoD.Offset".

-- -- | The shape should be placed at the endpoints of lines. 'Default' is
-- --   'LineCapButt'.
-- data LineCap
--   = LineCapButt   -- ^ Lines end precisely at their endpoints.
--   | LineCapRound  -- ^ Lines are capped with semicircles centered on
--                   --   endpoints.
--   | LineCapSquare -- ^ Lines are capped with a squares centered on
--                   --   endpoints.
--   deriving (Eq, Ord, Show, Typeable)
--   -- XXX PICS!!!

-- instance Default LineCap where
--   def = LineCapButt

-- -- | Last semigroup structure.
-- instance Semigroup LineCap where
--   _ <> b = b

_LineCap :: Equality' LineCap LineCap
_LineCap = id

-- | Set the line end cap attribute.
lineCap :: ApplyStyle a => LineCap -> a -> a
lineCap = applyAttr _LineCap

-- | Lens onto the line cap in a style.
_lineCap :: HasStyle a => Lens' a (Maybe LineCap)
_lineCap = style . atAttr _LineCap

-- line join -----------------------------------------------------------

-- 'LineJoin' is now defined in "Geometry.TwoD.Offset".

-- -- | How should the join points between line segments be drawn?
-- data LineJoin
--   = LineJoinMiter    -- ^ Use a \"miter\" shape (whatever that is).
--   | LineJoinRound    -- ^ Use rounded join points.
--   | LineJoinBevel    -- ^ Use a \"bevel\" shape (whatever that is).  Are
--                      --   these... carpentry terms?
--   deriving (Eq, Ord, Show, Typeable)
--   --- XXX MOAR PICS

_LineJoin :: Equality' LineJoin LineJoin
_LineJoin = id

-- | Last semigroup structure.
-- instance Semigroup LineJoin where
--   _ <> b = b

-- instance Default LineJoin where
--   def = LineJoinMiter

-- | Set the segment join style.
lineJoin :: ApplyStyle a => LineJoin -> a -> a
lineJoin = applyAttr _LineJoin
{-# INLINE lineJoin #-}

-- | Lens onto the line join type in a style.
_lineJoin :: HasStyle a => Lens' a (Maybe LineJoin)
_lineJoin = style . atAttr _LineJoin
{-# INLINE _lineJoin #-}

-- miter limit ---------------------------------------------------------

-- | Miter limit attribute affecting the 'LineJoinMiter' joins.
--   For some backends this value may have additional effects.
--   'Default' is @10@.
newtype LineMiterLimit = LineMiterLimit (Last Double)
  deriving (Typeable, Semigroup, Eq, Ord)

instance AttributeClass LineMiterLimit where
  type AttrType LineMiterLimit = 'IAttr

_LineMiterLimit :: Iso' LineMiterLimit Double
_LineMiterLimit = coerced

instance Default LineMiterLimit where
  def = LineMiterLimit (Last 10)

-- | Set the miter limit for joins with 'LineJoinMiter'.
lineMiterLimit :: ApplyStyle a => Double -> a -> a
lineMiterLimit = applyAttr _LineMiterLimit
{-# INLINE lineMiterLimit #-}

-- | Lens onto the line miter limit in a style.
_lineMiterLimit :: HasStyle a => Lens' a (Maybe Double)
_lineMiterLimit = style . atAttr _LineMiterLimit
{-# INLINE _lineMiterLimit #-}

-- Annotations ---------------------------------------------------------

-- | The opacity of a whole group of items.
newtype GroupOpacity = GroupOpacity Double
  deriving (Show, Typeable)

instance AnnotationClass GroupOpacity where
  type AnnotType GroupOpacity = 'IAnnot

-- | 'GroupOpacity' isomorphism.
_GroupOpacity :: Iso' GroupOpacity Double
_GroupOpacity = coerced
{-# INLINE _GroupOpacity #-}

-- | Set the opacity of diagram as a group.
groupOpacity :: Double -> Diagram v -> Diagram v
groupOpacity = applyAnnot _GroupOpacity
{-# INLINE groupOpacity #-}

-- | A hyperlink reference for the diagram.
newtype HRef = HRef String
  deriving (Show, Typeable)

instance AnnotationClass HRef where
  type AnnotType HRef = 'IAnnot

-- | 'HRef' isomorphism.
_HRef :: Iso' HRef String
_HRef = coerced
{-# INLINE _HRef #-}

-- | Add a hyperlink reference to the diagram.
href :: String -> Diagram v -> Diagram v
href = applyAnnot _HRef
{-# INLINE href #-}

