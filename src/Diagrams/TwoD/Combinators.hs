{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Combinators
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Diagram combinators specialized to two dimensions. For more general
-- combinators, see "Diagrams.Combinators".
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Combinators
  (
    -- * Binary combinators

    (===), (|||)

    -- * n-ary combinators
  , hcat, hsep, hsepEven
  , vcat, vsep, vsepEven

    -- * Spacing/envelopes
  , strutR2
  , strutX, strutY
  , padX, padY

  , extrudeLeft, extrudeRight, extrudeBottom, extrudeTop

  , rectEnvelope

  , boundingRect, bg, bgFrame

  ) where

import           Control.Lens              ((&), (.~))
import           Data.Colour
import           Data.Monoid.WithSemigroup
import           Linear.Vector

import           Geometry

import           Diagrams.Attributes
import           Diagrams.Combinators
import           Diagrams.TwoD.Attributes
import           Diagrams.Types
import           Diagrams.Util

-- | @strutR2 v@ is a two-dimensional diagram which produces no
--   output, but with respect to alignment, envelope, /and trace/ acts
--   like a 1-dimensional segment oriented along the vector @v@, with
--   local origin at its center.  If you don't care about the trace
--   then there's no difference between @strutR2@ and the more general
--   'strut'.
strutR2 :: (RealFloat n, Monoid' m) => V2 n -> QDiagram V2 n m
strutR2 v = phantom seg
  where
    seg = FLinear (origin .+^ 0.5 *^ v) (origin .+^ (-0.5) *^ v)

-- | @strutX w@ is an empty diagram with width @w@, height 0, and a
--   centered local origin.  Note that @strutX (-w)@ behaves the same as
--   @strutX w@.
strutX
  :: (HasLinearMap v, R1 v, OrderedField n, Monoid m)
  => n -> QDiagram v n m
strutX d = strut (zero & _x .~ d)
{-# INLINE strutX #-}

-- | @strutY h@ is an empty diagram with height @h@, width 0, and a
--   centered local origin. Note that @strutY (-h)@ behaves the same as
--   @strutY h@.
strutY
  :: (HasLinearMap v, R2 v, OrderedField n, Monoid m)
  => n -> QDiagram v n m
strutY d = strut (zero & _y .~ d)
{-# INLINE strutY #-}

-- | @padX s@ \"pads\" a diagram in the x-direction, expanding its
--   envelope horizontally by a factor of @s@ (factors between 0 and 1
--   can be used to shrink the envelope).  Note that the envelope will
--   expand with respect to the local origin, so if the origin is not
--   centered horizontally the padding may appear \"uneven\".  If this
--   is not desired, the origin can be centered (using 'centerX')
--   before applying @padX@.
padX :: (HasLinearMap v, R1 v, OrderedField n)
     => n -> QDiagram v n m -> QDiagram v n m
padX s d = withEnvelope (scaleX s $ getEnvelope d) d
{-# INLINE padX #-}

-- | @padY s@ \"pads\" a diagram in the y-direction, expanding its
--   envelope vertically by a factor of @s@ (factors between 0 and 1 can
--   be used to shrink the envelope).  Note that the envelope will
--   expand with respect to the local origin, so if the origin is not
--   centered vertically the padding may appear \"uneven\".  If this is
--   not desired, the origin can be centered (using 'centerY') before
--   applying @padY@.
padY :: (HasLinearMap v, R2 v, OrderedField n)
     => n -> QDiagram v n m -> QDiagram v n m
padY s d = withEnvelope (scaleY s $ getEnvelope d) d
{-# INLINE padY #-}

-- | @extrudeLeft s@ \"extrudes\" a diagram in the negative x-direction,
--   offsetting its envelope by the provided distance. When @ s < 0 @,
--   the envelope is inset instead.
--
--   See the documentation for 'extrudeEnvelope' for more information.
extrudeLeft
  :: OrderedField n
  => n
  -> QDiagram V2 n m
  -> QDiagram V2 n m
extrudeLeft s
  | s >= 0    = extrudeEnvelope $ unitX ^* negate s
  | otherwise = intrudeEnvelope $ unitX ^* negate s
{-# INLINE extrudeLeft #-}

-- | @extrudeRight s@ \"extrudes\" a diagram in the positive x-direction,
--   offsetting its envelope by the provided distance. When @ s < 0 @,
--   the envelope is inset instead.
--
--   See the documentation for 'extrudeEnvelope' for more information.
extrudeRight
  :: OrderedField n
  => n
  -> QDiagram V2 n m
  -> QDiagram V2 n m
extrudeRight s
  | s >= 0    = extrudeEnvelope $ unitX ^* s
  | otherwise = intrudeEnvelope $ unitX ^* s

-- | @extrudeBottom s@ \"extrudes\" a diagram in the negative y-direction,
--   offsetting its envelope by the provided distance. When @ s < 0 @,
--   the envelope is inset instead.
--
--   See the documentation for 'extrudeEnvelope' for more information.
extrudeBottom
  :: OrderedField n
  => n
  -> QDiagram V2 n m
  -> QDiagram V2 n m
extrudeBottom s
  | s >= 0    = extrudeEnvelope $ unitY ^* negate s
  | otherwise = intrudeEnvelope $ unitY ^* negate s

-- | @extrudeTop s@ \"extrudes\" a diagram in the positive y-direction,
--   offsetting its envelope by the provided distance. When @ s < 0 @,
--   the envelope is inset instead.
--
--   See the documentation for 'extrudeEnvelope' for more information.
extrudeTop
  :: OrderedField n
  => n
  -> QDiagram V2 n m
  -> QDiagram V2 n m
extrudeTop s
  | s >= 0    = extrudeEnvelope $ unitY ^* s
  | otherwise = intrudeEnvelope $ unitY ^* s

-- | @rectEnvelope p v@ sets the envelope of a diagram to a rectangle
--   whose lower-left corner is at @p@ and whose upper-right corner is
--   at @p .+^ v@.  Useful for selecting the rectangular portion of a
--   diagram which should actually be \"viewed\" in the final render, if
--   you don't want to see the entire diagram.
rectEnvelope
  :: forall n m. OrderedField n
  => Point V2 n -> V2 n -> QDiagram V2 n m -> QDiagram V2 n m
rectEnvelope p (V2 w h) = withEnvelope (rect w h # alignBL # moveTo p :: Path V2 n)

-- | Construct a bounding rectangle for an enveloped object, that is,
--   the smallest axis-aligned rectangle which encloses the object.
boundingRect
  :: (InSpace V2 n a, SameSpace a t, Enveloped t, FromTrail t, Transformable t, Monoid t, Enveloped a)
  => a -> t
boundingRect = (`boxFit` unitSquare) . boundingBox

-- | \"Set the background color\" of a diagram.  That is, place a
--   diagram atop a bounding rectangle of the given color.
bg :: Colour Double -> Diagram V2 -> Diagram V2
bg c d = d <> boundingRect d # lwO 0 # fc c

-- | Similar to 'bg' but makes the colored background rectangle larger than
--   the diagram. The first parameter is used to set how far the background
--   extends beyond the diagram.
bgFrame :: Double -> Colour Double -> Diagram V2 -> Diagram V2
bgFrame f c d = d <> boundingRect (frame f d) # lwO 0 # fc c

