{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Animation.Active
-- Copyright   :  (c) 2011-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
--
-- A few utilities and class instances for 'Active' (from the @active@
-- package).  In particular, this module defines
--
--   * An instance of 'V' for 'Active': @'V' ('Active' a) = 'V' a@
--
--   * 'HasOrigin', 'Transformable', and 'HasStyle' instances for
--     'Active' which all work pointwise.
--
--   * A 'TrailLike' instance for @'Active' t@ where @t@ is also
--     'TrailLike', which simply lifts a pathlike thing to an
--     (infinite) constant active value.
--
--   * 'Juxtaposable' instances for @'Active' a@ where @a@ is also
--     'Juxtaposable'.  An active value can be juxtaposed against
--     another by doing the juxtaposition pointwise over time.

-----------------------------------------------------------------------------

module Diagrams.Animation.Active
  ( activeEnvelope
  , activeBoundingBox
  ) where

import           Control.Applicative

import           Geometry
import           Active
import           Diagrams.Types.Style

type instance V (Active a) = V a
type instance N (Active a) = N a

-- Another option would be to put these in geometry. Active doesn't
-- contain any extra dependencies so I see no problem with this.

-- | Like 'animEnvelope', but with an adjustible sample rate.  The first
--   parameter is the number of samples per time unit to use.  Lower
--   rates will be faster but less accurate; higher rates are more
--   accurate but slower.

-- | Automatically assign fixed a envelope to the entirety of an
--   animation by sampling the envelope at a number of points in time
--   and taking the union of all the sampled envelopes to form the
--   \"hull\".  This hull is then used uniformly throughout the
--   animation.
--
--   This function can be very slow because every envelope query needs
--   to traverse every sample taken. See 'activeBoundingBox' for
--   constructing a bounding box which is /much/ faster to query.
activeEnvelope
  :: (InSpace v n a, Enveloped a)
  => Rational -> Active a -> Envelope v n
activeEnvelope r a = getEnvelope (samples r a)

-- | Get the bounding box by taking @r@ samples per time unit.
activeBoundingBox
  :: (InSpace v n a, HasBasis v, Enveloped a)
  => Rational -> Active a -> BoundingBox v n
activeBoundingBox r a = foldMap boundingBox (samples r a)

instance HasOrigin a => HasOrigin (Active a) where
  moveOriginTo = fmap . moveOriginTo

instance Transformable a => Transformable (Active a) where
  transform = fmap . transform

instance ApplyStyle a => ApplyStyle (Active a) where
  applyStyle = fmap . applyStyle

instance FromTrail t => FromTrail (Active t) where
  fromLocTrail = pure . fromLocTrail

-- | An active value can be juxtaposed against another by doing the
-- juxtaposition pointwise over time.
instance Juxtaposable a => Juxtaposable (Active a) where
  juxtapose = liftA2 . juxtapose

