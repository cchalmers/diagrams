{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Attributes
-- Copyright   :  (c) 2013-2015 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Diagrams may have /attributes/ which affect the way they are
-- rendered. This module defines /Textures/ (Gradients and Colors) in two
-- dimensions. Like the attributes defined in the Diagrams.Attributes module,
-- all attributes defined here use the 'Last' or /semigroup/ structure.
-- 'FillColor' and 'LineColor' attributes are provided so that backends that
-- don't support gradients need not be concerned with using textures. Backends
-- should only implement color attributes or textures attributes, not both.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Attributes (
  -- * Textures
    Texture(..)
  , ToTexture (..)
  , _SC, _AC, _LG, _RG
  , solid
  , GradientStop(..), stopColor, stopFraction, mkStops
  , SpreadMethod(..)

  -- ** Gradients
  , HasGradient (..)

  -- *** Linear Gradients
  , LGradient
  , mkLinearGradient
  , gradientStart
  , gradientEnd

  -- *** Radial Gradients
  , RGradient
  , mkRadialGradient
  , gradientCenter0, gradientRadius0
  , gradientCenter1, gradientRadius1

  -- ** Line texture
  , LineTexture, _LineTexture
  , lineTexture, _lineTexture

  -- ** Line color
  , lineColor, lc, lcA

  -- ** Fill texture
  , FillTexture, _FillTexture
  , fillTexture, _fillTexture

  -- ** Fill color
  , fillColor, fc, fcA
  , backupFillColor

  -- * Annotations
  -- ** Clip
  , Clip
  , _Clip
  , clip
  , multiClip

  -- ** Shading
  , Shading
  , _Shading
  , shading

  ) where

import           Control.Lens          hiding (transform)
import           Data.Colour           hiding (AffineSpace, over)
import           Data.Data
import           Data.Default.Class
import qualified Data.Foldable         as F
import           Data.Semigroup
import qualified Data.Sequence         as Seq

import           Geometry.Path
import           Geometry.Query
import           Geometry.Space
import           Geometry.Transform
import           Geometry.TwoD.Path    (isInsideWinding)
import           Geometry.TwoD.Types   hiding (p2)

import           Diagrams.Attributes
import           Diagrams.Types

import           Data.Coerce

-----------------------------------------------------------------------------
-- Gradients
-----------------------------------------------------------------------------

-- | A gradient stop contains a color and fraction (usually between 0 and 1)
data GradientStop = GradientStop
  { _stopColor    :: !(AlphaColour Double)
  , _stopFraction :: !Double
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''GradientStop

-- | A color for the stop.
stopColor :: Lens' GradientStop (AlphaColour Double)

-- | The fraction for stop.
stopFraction :: Lens' GradientStop Double

-- | A convenient function for making gradient stops from a list of triples.
--   (An opaque color, a stop fraction, an opacity).
mkStops :: Color c => [(c, Double)] -> [GradientStop]
mkStops = map (\(c, x) -> GradientStop (toAlphaColour c) x)

-- | The 'SpreadMethod' determines what happens before 'lGradStart' and
--   after 'lGradEnd'. 'GradPad' fills the space before the start of the
--   gradient with the color of the first stop and the color after end
--   of the gradient with the color of the last stop. 'GradRepeat'
--   restarts the gradient and 'GradReflect' restarts the gradient with
--   the stops in reverse order.
data SpreadMethod
  = GradPad     -- ^ <diagram>
  | GradReflect -- ^ <diagram>
  | GradRepeat  -- ^ <diagram>

-- | Class of lenses both 'LGradient' and 'RGradient' use.
class InSpace V2 Double a => HasGradient a where
  -- | The colours and the positions of the colours.
  gradientStops :: Lens' a [GradientStop]

  -- | The 'SpreadMethod' to use for the gradient.
  gradientSpreadMethod :: Lens' a SpreadMethod

  -- | A transformation to be applied to the gradient. Usually this field
  --   will start as the identity transform and capture the transforms
  --   that are applied to the gradient.
  --
  --   Note that 'LGradient' and 'RGradient' also have 'Transformable'
  --   instances.
  gradientTransform :: Lens' a (T2 Double)

-- Linear gradients ----------------------------------------------------

--     - 'gradientStart'        :: 'Lens'' 'LGradient' ('P2' 'Double')
--     - 'gradientEnd'          :: 'Lens'' 'LGradient' ('P2' 'Double')
--     - 'gradientStops'        :: 'Lens'' 'LGradient' ['GradientStop']
--     - 'gradientTranform'     :: 'Lens'' 'LGradient' ('T2' 'Double')
--     - 'gradientSpreadMethod' :: 'Lens'' 'LGradient' 'SpreadMethod'

-- | Linear Gradient. A linear gradient is created with
--   'mkLinearGradient'. A linear gradient supports the following
--   lenses:
--
--     - 'gradientStart': Starting point of gradient
--     - 'gradientEnd': Ending point of gradient
--     - 'gradientStops': The colours that make up the gradient
--     - 'gradientTranform': The transform applied to the gradient
--     - 'gradientSpreadMethod': How the gradient fills space beyond the
--        start and end points
--
--  You can use 'fillTexture' or 'lineTexture' to apply a gradient to a
--  diagram.
data LGradient = LGradient
  { _gradientStart         :: P2 Double
  , _gradientEnd           :: P2 Double
  , _lGradStops            :: [GradientStop]
  , _lGradTransform    :: T2 Double
  , _lGradSpreadMethod :: SpreadMethod
  }

type instance V LGradient = V2
type instance N LGradient = Double

makeLensesWith (lensRules & generateSignatures .~ False) ''LGradient

instance Transformable LGradient where
  transform = over lGradTransform . transform

instance HasGradient LGradient where
  gradientStops        = lGradStops
  gradientTransform    = lGradTransform
  gradientSpreadMethod = lGradSpreadMethod

lGradStops :: Lens' LGradient [GradientStop]
lGradTransform :: Lens' LGradient (T2 Double)
lGradSpreadMethod :: Lens' LGradient SpreadMethod

-- | The staring point for an 'LGradient'.
gradientStart :: Lens' LGradient (P2 Double)

-- | The finishing point for an 'LGradient'.
gradientEnd :: Lens' LGradient (P2 Double)

-- | Make a radial gradient texture from a stop list and start and end
--   points. The 'gradientSpreadMethod' is 'GradPad'.
mkLinearGradient
  :: Color c
  => [(c, Double)] -- ^ colours to use
  -> P2 Double     -- ^ start point
  -> P2 Double     -- ^ end point
  -> LGradient     -- ^ linear gradient texture
mkLinearGradient cs p1 p2  = LGradient
  { _gradientStart     = p1
  , _gradientEnd       = p2
  , _lGradStops        = mkStops cs
  , _lGradTransform    = mempty
  , _lGradSpreadMethod = GradPad
  }

-- Radial gradients ----------------------------------------------------

-- | Radial Gradient. A radial gradient is created with
--   'mkRadialGradient'. A radial gradient supports the following
--   lenses:
--
--     - 'gradientCenter0': Inner circle center
--     - 'gradientRadius0': Inner circle radius (default 0)
--     - 'gradientCenter1': Outer circle center
--     - 'gradientRadius1': Outer circle radius
--     - 'gradientStops': The colours that make up the gradient
--     - 'gradientTransform': The transform applied to the gradient
--     - 'gradientSpreadMethod': How the gradient fills space beyond the
--        outer circle
--
--  You can use 'fillTexture' or 'lineTexture' to apply a gradient to a
--  diagram.
data RGradient = RGradient
  { _gradientCenter0   :: P2 Double
  , _gradientRadius0   :: Double
  , _gradientCenter1   :: P2 Double
  , _gradientRadius1   :: Double
  , _rGradStops        :: [GradientStop]
  , _rGradSpreadMethod :: SpreadMethod
  , _rGradTrans        :: T2 Double
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''RGradient

type instance V RGradient = V2
type instance N RGradient = Double

instance Transformable RGradient where
  transform = over rGradTrans . transform

instance HasGradient RGradient where
  gradientStops        = rGradStops
  gradientTransform    = rGradTrans
  gradientSpreadMethod = rGradSpreadMethod

rGradStops :: Lens' RGradient [GradientStop]
rGradTrans :: Lens' RGradient (T2 Double)
rGradSpreadMethod :: Lens' RGradient SpreadMethod

-- | The center point of the inner circle.
gradientCenter0 :: Lens' RGradient (P2 Double)

-- | The radius of the inner cirlce in 'local' coordinates.
gradientRadius0 :: Lens' RGradient Double

-- | The center of the outer circle.
gradientCenter1  :: Lens' RGradient (P2 Double)

-- | The radius of the outer circle in 'local' coordinates.
gradientRadius1 :: Lens' RGradient Double

-- | Make a radial gradient texture from a stop list, center point and
--   outer circle radius. Both the inner and outer circles use the same
--   center. The radius of the inner circle is 0. The
--   'gradientSpreadMethod' is 'GradPad'.
mkRadialGradient
  :: Color c
  => [(c, Double)]
  -> P2 Double
  -> Double
  -> RGradient
mkRadialGradient stops c1 r1 = RGradient
  { _gradientCenter0   = mkP2 0 0
  , _gradientRadius0   = 0.0
  , _gradientCenter1   = c1
  , _gradientRadius1   = r1
  , _rGradStops        = mkStops stops
  , _rGradTrans        = mempty
  , _rGradSpreadMethod = GradPad
  }

------------------------------------------------------------------------
-- Textures
------------------------------------------------------------------------

-- | A Texture is either a color 'SC', linear gradient 'LG', or radial
--   gradient 'RG'. An object can have only one texture which is
--   determined by the 'Last' semigroup structure.
data Texture = SC SomeColor | LG LGradient | RG RGradient
  deriving Typeable

type instance V Texture = V2
type instance N Texture = Double

makePrisms ''Texture

-- | Prism onto an 'AlphaColour' 'Double' of a 'SC' texture.
_AC :: Prism' Texture (AlphaColour Double)
_AC = _SC . _SomeColor

instance Transformable Texture where
  transform t (LG lg) = LG $ transform t lg
  transform t (RG rg) = RG $ transform t rg
  transform _ sc      = sc

-- | Convert a solid colour into a texture.
solid :: Color a => a -> Texture
solid = SC . SomeColor

-- | Class of things that can be used as a 2D texture.
class ToTexture a where
  -- | Convert to a texture.
  toTexture :: a -> Texture

instance ToTexture Texture where
  toTexture = id

instance a ~ Double => ToTexture (Colour a) where
  toTexture = solid

instance a ~ Double => ToTexture (AlphaColour a) where
  toTexture = solid

instance ToTexture SomeColor where
  toTexture = SC

instance ToTexture LGradient where
  toTexture = LG

instance ToTexture RGradient where
  toTexture = RG

-- Line Texture --------------------------------------------------------

-- | The texture with which lines are drawn.  Note that child
--   textures always override parent textures.
--   More precisely, the semigroup structure on line texture attributes
--   is that of 'Last'.
newtype LineTexture = LineTexture (Last Texture)
  deriving (Typeable, Semigroup)

instance AttributeClass LineTexture where
  type AttrType LineTexture = 'TAttr

type instance V LineTexture = V2
type instance N LineTexture = Double

-- | Isomorphism bettern a line texture and a 'Texture'.
_LineTexture :: Iso' LineTexture Texture
_LineTexture = coerced

-- Only gradients get transformed. The transform is applied to the gradients
-- transform field. Colors are left unchanged.
instance Transformable LineTexture where
  transform = coerce (transform :: T2 Double -> Texture -> Texture)

instance Default LineTexture where
  def = review _LineTexture (toTexture black)

-- | Apply a line texture.
--
-- @
-- 'lineTexture' :: 'Colour' 'Double'      -> 'Diagram' 'V2' -> 'Diagram' 'V2'
-- 'lineTexture' :: 'AlphaColour' 'Double' -> 'Diagram' 'V2' -> 'Diagram' V2
-- 'lineTexture' :: 'SomeColor'          -> 'Diagram' 'V2' -> 'Diagram' 'V2'
-- 'lineTexture' :: 'Texture'            -> 'Diagram' 'V2' -> 'Diagram' 'V2'
-- 'lineTexture' :: 'RGradient'          -> 'Diagram' 'V2' -> 'Diagram' 'V2'
-- 'lineTexture' :: 'LGradient'          -> 'Diagram' 'V2' -> 'Diagram' 'V2'
-- @
lineTexture :: (InSpace V2 Double a, ToTexture t, ApplyStyle a) => t -> a -> a
lineTexture = applyAttr _LineTexture . toTexture

_lineTexture :: (InSpace V2 Double a, HasStyle a) => Lens' a (Maybe Texture)
_lineTexture = style . atAttr _LineTexture

-- | Set the line (stroke) color.  This function is polymorphic in the
--   color type (so it can be used with either 'Colour' or
--   'AlphaColour'), but this can sometimes create problems for type
--   inference, so the 'lc' and 'lcA' variants are provided with more
--   concrete types.
lineColor :: (InSpace V2 Double a, Color c, ApplyStyle a) => c -> a -> a
lineColor = lineTexture . SomeColor

-- | A synonym for 'lineColor', specialized to @'Colour' Double@
--   (i.e. opaque colors).  See comment in 'lineColor' about backends.
lc :: (InSpace V2 Double a, ApplyStyle a) => Colour Double -> a -> a
lc = lineColor

-- | A synonym for 'lineColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency).  See comment in 'lineColor'
--   about backends.
lcA :: (InSpace V2 Double a, ApplyStyle a) => AlphaColour Double -> a -> a
lcA = lineColor

-- Fill Texture --------------------------------------------------------

-- | The texture with which objects are filled. The semigroup structure
--   on fill texture attributes is that of 'Last'.
newtype FillTexture = FillTexture (Last Texture)
  deriving (Typeable, Semigroup)

instance AttributeClass FillTexture where
  type AttrType FillTexture = 'TAttr

-- | Isomorphism bettern a fill texture and a 'Texture'.
_FillTexture :: Iso' FillTexture Texture
_FillTexture = coerced

type instance V FillTexture = V2
type instance N FillTexture = Double

-- Only gradients get transformed. The transform is applied to the gradients
-- transform field. Colors are left unchanged.
instance Transformable FillTexture where
  transform = over _FillTexture . transform
  {-# INLINE transform #-}

instance Default FillTexture where
  def = review (_FillTexture . _AC) transparent

-- | Apply a fill texture to something.
--
-- @
-- 'fillTexture' :: 'Colour' 'Double'      -> 'Diagram' 'V2' -> 'Diagram' 'V2'
-- 'fillTexture' :: 'AlphaColour' 'Double' -> 'Diagram' 'V2' -> 'Diagram' 'V2'
-- 'fillTexture' :: 'SomeColor           -> 'Diagram' 'V2' -> 'Diagram' 'V2'
-- 'fillTexture' :: 'LGradient'          -> 'Diagram' 'V2' -> 'Diagram' 'V2'
-- 'fillTexture' :: 'RGradient'          -> 'Diagram' 'V2' -> 'Diagram' 'V2'
-- 'fillTexture' :: 'Texture'            -> 'Diagram' 'V2' -> 'Diagram' 'V2'
-- @
fillTexture :: (InSpace V2 Double a, ToTexture t, ApplyStyle a) => t -> a -> a
fillTexture = applyAttr _FillTexture . toTexture

-- | Lens onto a fill texture in a style.
--
-- @
-- '_fillTexture' :: 'Lens'' ('Style' 'V2' 'Double') ('Maybe' 'Texture')
-- @
_fillTexture
  :: (InSpace V2 Double a, HasStyle a) => Lens' a (Maybe Texture)
_fillTexture = style . atAttr _FillTexture

-- | Set the fill color. This function is polymorphic in the color
--   type (so it can be used with either 'Colour' or 'AlphaColour'),
--   but this can sometimes create problems for type inference, so the
--   'fc' and 'fcA' variants are provided with more concrete types.
fillColor :: (InSpace V2 Double a, Color c, ApplyStyle a) => c -> a -> a
fillColor = fillTexture . SomeColor

-- | Set a \"backup\" fill color, to be used only if no explicit
--   calls to 'fillColor' (or 'fc', or 'fcA') are used.
--   See comment after 'fillColor' about backends.
backupFillColor
  :: (InSpace V2 Double a, Color c, ApplyStyle a)
  => c -> a -> a
backupFillColor = applyBackupAttr _FillTexture . toTexture . SomeColor

-- | A synonym for 'fillColor', specialized to @'Colour' Double@
--   (i.e. opaque colors). See comment after 'fillColor' about backends.
fc :: (InSpace V2 Double a, ApplyStyle a) => Colour Double -> a -> a
fc = fillColor

-- | A synonym for 'fillColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency). See comment after 'fillColor' about backends.
fcA :: (InSpace V2 Double a, ApplyStyle a) => AlphaColour Double -> a -> a
fcA = fillColor

------------------------------------------------------------------------
-- Annotations
------------------------------------------------------------------------

-- Clip ----------------------------------------------------------------

-- | A clip is a list of paths that make up the clip.
newtype Clip = Clip (Seq.Seq (Path V2 Double))
  deriving (Semigroup, Monoid, Typeable)
-- Note: It is possible to represent a clip as a single path by taking
-- the intersection of each path, but this seems simpler.

type instance V Clip = V2
type instance N Clip = Double

instance Transformable Clip where
  transform = over _Clip . transform
  {-# INLINE transform #-}

-- | A point inside a clip if the point is in 'All' individual clipping
--   paths.
instance HasQuery Clip All where
  getQuery (Clip paths) = Query $ \p ->
    F.foldMap (All . flip isInsideWinding p) paths

instance AnnotationClass Clip where
  type AnnotType Clip = 'TAnnot

_Clip :: Iso' Clip (Seq.Seq (Path V2 Double))
_Clip = coerced
{-# INLINE _Clip #-}

clip :: Path V2 Double -> Diagram V2 -> Diagram V2
clip = multiClip . Seq.singleton
{-# INLINE clip #-}

multiClip :: Seq.Seq (Path V2 Double) -> Diagram V2 -> Diagram V2
multiClip = applyAnnot _Clip
{-# INLINE multiClip #-}

-- Shading -------------------------------------------------------------

-- | A shading is using the luminosity of the one diagram as the alpha
--   channel for the other.
newtype Shading = Shading (Diagram V2)
  deriving (Semigroup, Monoid, Typeable, Transformable)

type instance V Shading = V2
type instance N Shading = Double

instance AnnotationClass Shading where
  type AnnotType Shading = 'TAnnot

_Shading :: Iso' Shading (Diagram V2)
_Shading = coerced
{-# INLINE _Shading #-}

-- | @'shading' a b@ using the luminosity of diagram @a@ as the alpha
--   channel for diagram @b@.
--
--   Not supported on all backends.
shading :: Diagram V2 -> Diagram V2 -> Diagram V2
shading = applyAnnot _Shading
{-# INLINE shading #-}

------------------------------------------------------------------------
-- Gradient calculations
------------------------------------------------------------------------

-- -- $gradient-calculations
-- -- Calculations for gradients. These are mostly used for backends.

-- -- | Calculate the correct linear stops such that the path is completely
-- --   filled. PGF doesn't have spread methods so this has to be done
-- --   manually.
-- calcLinearStops
--   :: Envelope V2 Double  -- ^ envelope of object being covered
--   -> LGradient
--   -> ([GradientStop], T2 Double)
-- calcLinearStops EmptyEnvelope _ = ([], mempty)
-- calcLinearStops env (LGradient stops gt pend sm)
--   = (linearStops' x0 x1 stops sm, t <> ft)
--   where
--     -- Transform such that the transform t origin is start of the
--     -- gradient, transform t unitX is the end.
--     t = gt
--         -- encorperate the start and end points
--      -- <> translation (p0 ^. _Point)
--      -- <> scaling (norm (p1 .-. p0))
--      -- <> rotationTo (dirBetween p1 p0)

--     -- Use the inverse transformed path and make the pre-transformed
--     -- gradient fit to it. Then when we transform the gradient we know
--     -- it'll fit the path.
--     env'         = transform (inv t) env
--     Just (x0,x1) = extentX env'
--     Just (y0,y1) = extentY env'

--     -- Final transform to fit the gradient to the path. The origin on
--     -- the gradient is its centre so we translate by - V2 50 50 to get
--     -- to the lower corner (because of this we set the size of the
--     -- gradient to always be 100 x 100 for simplicity). Then scales up
--     -- the gradient to cover the path and moves it into position.
--     ft = translation (V2 x0 y0) <> scalingV ((*0.01) . abs <$> V2 (x0 - x1) (y0 - y1)) <> translation 50



-- linearStops' :: RealFloat n
--              => n -> n -> [GradientStop n] -> SpreadMethod -> [GradientStop n]
-- linearStops' x0 x1 stops sm =
--   GradientStop c1' 0 : filter (inRange . view stopFraction) stops' ++ [GradientStop c2' 100]
--   where
--     stops' = case sm of
--       GradPad     -> over (each . stopFraction) normalise stops
--       GradRepeat  -> flip F.foldMap [i0 .. i1] $ \i ->
--                        increaseFirst $
--                          over (each . stopFraction)
--                               (normalise . (+ fromIntegral i))
--                               stops
--       GradReflect -> flip F.foldMap [i0 .. i1] $ \i ->
--                        over (each . stopFraction)
--                             (normalise . (+ fromIntegral i))
--                             (reverseOdd i stops)

--     -- for repeat it sometimes complains if two are exactly the same so
--     -- increase the first by a little
--     increaseFirst = over (_head . stopFraction) (+0.001)
--     reverseOdd i
--       | odd i     = reverse . over (each . stopFraction) (1 -)
--       | otherwise = id
--     i0 = floor x0 :: Int
--     i1 = ceiling x1
--     c1' = SomeColor $ colourInterp stops' 0
--     c2' = SomeColor $ colourInterp stops' 100
--     inRange x   = x > 0 && x < 100
--     normalise x = 100 * (x - x0) / (x1 - x0)

-- colourInterp :: [GradientStop] -> Double -> AlphaColour Double
-- colourInterp cs0 x = go cs0
--   where
--     go (GradientStop c1 a : c@(GradientStop c2 b) : cs)
--       | x <= a         = toAlphaColour c1
--       | x > a && x < b = blend y (toAlphaColour c2) (toAlphaColour c1)
--       | otherwise      = go (c : cs)
--       where
--         y = realToFrac $ (x - a) / (b - a)
--     go [GradientStop c2 _] = toAlphaColour c2
--     go _ = transparent

-- -- | Calculate the correct linear stops such that the path is completely
-- --   filled. PGF doesn't have spread methods so this has to be done
-- --   manually.
-- calcRadialStops
--   :: Envelope V2 Double -> RGradient -> ([GradientStop], T2 Double, P2 Double)
-- calcRadialStops EmptyEnvelope _ = ([], mempty, origin)
-- calcRadialStops env (RGradient stops p0 r0 p1 r1 gt _sm)
--   = (stops', t <> ft, P cv)
--   where
--     cv = tp0 .-. tp1
--     tp0 = papply gt p0
--     tp1 = papply gt p1
--     -- Transform such that the transform t origin is start of the
--     -- gradient, transform t unitX is the end.
--     t = gt
--      <> translation (p1 ^. _Point)
--      <> scaling r1

--     -- Similar to linear gradients but not so precise, d is a (bad and
--     -- probably incorrect) lower bound for the required radius of the
--     -- circle to cover the path.
--     env'         = transform (inv t) env
--     Just (x0,x1) = extentX env'
--     Just (y0,y1) = extentY env'
--     d = 2 * max (max (abs $ x0 - x1) (abs $ y0 - y1)) (lstop ^. stopFraction)

--     -- Adjust for gradient size having radius 100
--     ft = scaling 0.01

--     -- Stops are scaled to start at r0 and end at r1. The gradient is
--     -- extended to d to try to cover the path.
--     --
--     -- The problem is extending the size of the gradient in this way
--     -- affects how the gradient scales if it is off-centre. This needs
--     -- to be fixed.
--     --
--     -- Only the GradPad spread method is supported for now.
--     stops' = head stops : over (each . stopFraction) refrac stops ++ [lstop & stopFraction .~ 100*d]
--     refrac x = 100 * ((r0 + x * (r1 - r0)) / r1) -- start at r0, end at r1
--     lstop = last stops


