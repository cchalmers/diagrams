{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Combinators
-- Copyright   :  (c) 2011-2018 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The core library of primitives forming the basis of an embedded
-- domain-specific language for describing and rendering diagrams.
--
-- "Diagrams.Core.Types" defines types and classes for
-- primitives, diagrams, and backends.
--
-----------------------------------------------------------------------------

module Diagrams.Combinators
  ( -- * Unary operations

    withEnvelope, withTrace
  , phantom, strut
  , pad, frame
  , extrudeEnvelope, intrudeEnvelope

    -- * Binary operations
  , beside
  , atDirection

    -- * n-ary operations
  , appends
  , position, atPoints
  , cat, sep, sepEven
  ) where

import           Control.Lens                       hiding (beside)
import           Data.Monoid.WithSemigroup

import           Geometry
import           Geometry.Direction
import           Linear                             (V3, negated)
import           Linear.Metric
import           Linear.Vector                      ((*^))

import           Diagrams.Types
import qualified Numeric.Interval.NonEmpty.Internal as I

------------------------------------------------------------
-- Working with envelopes
------------------------------------------------------------

-- | Use the envelope from some object as the envelope for a
--   diagram, in place of the diagram's default envelope.
--
--   <<diagrams/src_Diagrams_Combinators_withEnvelopeEx.svg#diagram=withEnvelopeEx&width=300>>
--
--   > sqNewEnv =
--   >     circle 1 # fc green
--   >     |||
--   >     (    c # dashingG [0.1,0.1] 0 # lc white
--   >       <> square 2 # withEnvelope (c :: Diagram V2) # fc blue
--   >     )
--   > c = circle 0.8
--   > withEnvelopeEx = sqNewEnv # centerXY # pad 1.5
withEnvelope
  :: (InSpace v n a, Enveloped a)
  => a -> QDiagram v n m -> QDiagram v n m
withEnvelope = replaceEnvelope . getEnvelope
{-# INLINE withEnvelope #-}

-- | Use the trace from some object as the trace for a diagram, in
--   place of the diagram's default trace.
withTrace :: (InSpace v n a, Traced a)
          => a -> QDiagram v n m -> QDiagram v n m
withTrace = modTrace . const . getTrace
{-# INLINE withTrace #-}

-- | @phantom x@ produces a \"phantom\" diagram, which has the same
--   envelope and trace as @x@ but produces no output.
phantom
  :: (InSpace v n a, Enveloped a, Traced a, Monoid' m)
  => a -> QDiagram v n m
phantom a = upDiagram $
  mempty & upEnvelope .~ getEnvelope a
         & upTrace    .~ getTrace a
{-# INLINE phantom #-}

-- | @pad s@ \"pads\" a diagram, expanding its envelope by a factor of
--   @s@ (factors between 0 and 1 can be used to shrink the envelope).
--   Note that the envelope will expand with respect to the local
--   origin, so if the origin is not centered the padding may appear
--   \"uneven\".  If this is not desired, the origin can be centered
--   (using, e.g., 'centerXY' for 2D diagrams) before applying @pad@.
pad :: (HasLinearMap v, OrderedField n)
    => n -> QDiagram v n m -> QDiagram v n m
pad s = modEnvelope (scale s)
{-# INLINE pad #-}

-- | @frame s@ increases the envelope of a diagram by and absolute
--   amount @s@, s is in the local units of the diagram. This function
--   is similar to @pad@, only it takes an absolute quantity and
--   pre-centering should not be necessary.
frame :: (Ord n, Fractional n) => n -> QDiagram v n m -> QDiagram v n m
frame s = modEnvelope $ onEnvelope (\f x -> inflate (f x))
  where
    inflate (I.I a b)
      | a' > b'   = I.singleton ((a+b)/2)
      | otherwise = I.I a' b'
      where
        a' = a - s
        b' = b + s
{-# SPECIALISE frame :: Double -> QDiagram v Double m -> QDiagram v Double m #-}

-- | @strut v@ is a diagram which produces no output, but with respect
--   to alignment and envelope acts like a 1-dimensional segment
--   oriented along the vector @v@, with local origin at its
--   center. (Note, however, that it has an empty trace; for 2D struts
--   with a nonempty trace see 'strutR2', 'strutX', and 'strutY' from
--   "Diagrams.TwoD.Combinators".) Useful for manually creating
--   separation between two diagrams.
--
--   <<diagrams/src_Diagrams_Combinators_strutEx.svg#diagram=strutEx&width=300>>
--
--   > strutEx = (circle 1 ||| strut unitX ||| circle 1) # centerXY # pad 1.1
strut :: (HasLinearMap v, OrderedField n, Monoid m) => v n -> QDiagram v n m
strut v = upWith (upEnvelope .~ env)
  where env = translate ((-0.5) *^ v) . getEnvelope $ straight v
  -- note we can't use 'phantom' here because it tries to construct a
  -- trace as well, and segments do not have a trace in general (only
  -- in 2D; see Diagrams.TwoD.Segment).  This is a good reason to have
  -- a special 'strut' combinator (before the introduction of traces
  -- it was mostly just for convenience).
  --
  -- also note that we can't remove the call to getEnvelope, since
  -- translating a segment has no effect.
{-# INLINEABLE [0] strut #-}
{-# SPECIALISE strut :: V2 Double -> Diagram V2 #-}
{-# SPECIALISE strut :: V3 Double -> Diagram V3 #-}

-- | @extrudeEnvelope v d@ asymmetrically \"extrudes\" the envelope of
--   a diagram in the given direction.  All parts of the envelope
--   within 90 degrees of this direction are modified, offset outwards
--   by the magnitude of the vector.
--
--   This works by offsetting the envelope distance proportionally to
--   the cosine of the difference in angle, and leaving it unchanged
--   when this factor is negative.
extrudeEnvelope
  :: (HasLinearMap v, OrderedField n)
  => v n -> QDiagram v n m -> QDiagram v n m
extrudeEnvelope v = deformEnvelope (norm v) $ dir v
{-# INLINEABLE [0] extrudeEnvelope #-}
{-# SPECIALISE extrudeEnvelope :: V2 Double -> Diagram V2 -> Diagram V2 #-}
{-# SPECIALISE extrudeEnvelope :: V3 Double -> Diagram V3 -> Diagram V3 #-}

-- | @intrudeEnvelope v d@ asymmetrically \"intrudes\" the envelope of
--   a diagram away from the given direction.  All parts of the envelope
--   within 90 degrees of this direction are modified, offset inwards
--   by the magnitude of the vector.
--
--   Note that this could create strange inverted envelopes, where
--   @ diameter v d < 0 @.
intrudeEnvelope
  :: (HasLinearMap v, OrderedField n)
  => v n -> QDiagram v n m -> QDiagram v n m
intrudeEnvelope v = deformEnvelope (-norm v) $ dir (negated v)
{-# INLINEABLE [0] intrudeEnvelope #-}
{-# SPECIALISE intrudeEnvelope :: V2 Double -> Diagram V2 -> Diagram V2 #-}
{-# SPECIALISE intrudeEnvelope :: V3 Double -> Diagram V3 -> Diagram V3 #-}

-- Utility for extrudeEnvelope / intrudeEnvelope
deformEnvelope
  :: (HasLinearMap v, OrderedField n)
  => n -> Direction v n -> QDiagram v n m -> QDiagram v n m
deformEnvelope s (Dir v) = modEnvelope (onEnvelope deformE)
  where
    deformE f (Dir v')
      | dp > 0    = f (Dir v') & \(I.I a b) -> I.I a (b + dp*s)
      | dp < 0    = f (Dir v') & \(I.I a b) -> I.I (a + dp*s) b
      | otherwise = f (Dir v')
      where dp = v' `dot` v
{-# INLINE deformEnvelope #-}
