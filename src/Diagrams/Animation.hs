{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Animation
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- An animation is a time-varying diagram, together with start and end
-- times.  Most of the tools for working with animations can actually
-- be found in the @active@ package, which defines the 'Active' type.
--
-- XXX more documentation and examples should go here
--
-----------------------------------------------------------------------------

module Diagrams.Animation
  ( -- * Animation combinators and tools
    -- $animComb

    Animation

  , animEnvelope, animEnvelope'

  , animRect, animRect'

  , fadeIn, fadeOut

  ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative       ((<$>))
import           Data.Foldable             (foldMap)
#endif
import           Data.Monoid
import           Data.Monoid.WithSemigroup

import           Active
import           Diagrams.Animation.Active
import           Diagrams.Attributes
import           Diagrams.Combinators
import           Diagrams.Types
import           Geometry

-- $animComb
-- Most combinators for working with animations are to be found in the
-- @active@ package, which defines the 'Active' type.  This module
-- defines just a few combinators specifically for working with
-- animated diagrams.

-- | An animation is just an 'Active', /i.e./ time-varying, diagram.
type Animation v = Active (QDiagram v Double Any)

-- It would be cool to have a variant of animEnvelope that tries to do
-- some sort of smart adaptive sampling to get good results more
-- quickly.  One could also imagine trying to use some sort of
-- automatic differentiation but that probably wouldn't work in all
-- cases we want to handle.

-- | Automatically assign fixed a envelope to the entirety of an
--   animation by sampling the envelope at a number of points in time
--   and taking the union of all the sampled envelopes to form the
--   \"hull\".  This hull is then used uniformly throughout the
--   animation.
--
--   This is useful when you have an animation that grows and shrinks
--   in size or shape over time, but you want it to take up a fixed
--   amount of space, /e.g./ so that the final rendered movie does not
--   zoom in and out, or so that it occupies a fixed location with
--   respect to another animation, when combining animations with
--   something like '|||'.
--
--   By default, 30 samples per time unit are used; to adjust this
--   number see 'animEnvelope''.
--
--   See also 'animRect' for help constructing a background to go
--   behind an animation.
animEnvelope
  :: (OrderedField n, HasLinearMap v, Monoid' m)
  => Active (QDiagram v n m) -> Active (QDiagram v n m)
animEnvelope = animEnvelope' 30

-- | Like 'animEnvelope', but with an adjustible sample rate.  The first
--   parameter is the number of samples per time unit to use.  Lower
--   rates will be faster but less accurate; higher rates are more
--   accurate but slower.
animEnvelope'
  :: (OrderedField n, HasLinearMap v, Monoid' m)
  => Rational -> Active (QDiagram v n m) -> Active (QDiagram v n m)
animEnvelope' r a = withEnvelope (samples r a) <$> a

-- | @animRect@ works similarly to 'animEnvelope' for 2D diagrams, but
--   instead of adjusting the envelope, simply returns the smallest
--   bounding rectangle which encloses the entire animation.  Useful
--   for /e.g./ creating a background to go behind an animation.
--
--   Uses 30 samples per time unit by default; to adjust this number
--   see 'animRect''.
animRect
  :: ( InSpace V2 n t, Monoid' m
     , FromTrail t, Enveloped t, Transformable t, Monoid t
     )
  => Active (QDiagram V2 n m) -> t
animRect = animRect' 30

-- | Like 'animRect', but with an adjustible sample rate.  The first
--   parameter is the number of samples per time unit to use.  Lower
--   rates will be faster but less accurate; higher rates are more
--   accurate but slower.
animRect'
  :: ( InSpace V2 n t, Monoid' m
     , FromTrail t, Enveloped t, Transformable t, Monoid t
     )
  => Rational -> Active (QDiagram V2 n m) -> t
animRect' r a = boxFit (activeBoundingBox r a) (square 1)

-- XXX make it take an Active Rational as parameter!
fadeIn
  :: (HasLinearMap v, Floating n, Ord n, Semigroup m)
  => Rational -> Active (QDiagram v n m -> QDiagram v n m)
fadeIn d = opacity . fromRational . (/d) <$> interval 0 d

-- XXX
fadeOut
  :: (HasLinearMap v, Floating n, Ord n, Semigroup m)
  => Rational -> Active (QDiagram v n m -> QDiagram v n m)
fadeOut = backwards . fadeIn

