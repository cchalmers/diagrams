{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeFamilies   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Measured
-- Copyright   :  (c) 2018 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- 'Measured' diagrams allow access to 'global', 'normalized, 'output'
-- and 'local' units when making a diagram (normal diagrams are in
-- 'local' units). The downside is we can't make use of the 'Envelope'
-- or 'Trace'.
--
-----------------------------------------------------------------------------
module Diagrams.Measured
  (
    -- * Measured diagrams
    MDiagram
  , measuredDiagram
  , unmeasureDiagram

    -- * Internals
  , measuredLeaf
  )
  where

import           Data.Monoid.Coproduct.Strict
import           Data.Typeable

import           Diagrams.Backend.Compile     (normalizedFactor)
import           Diagrams.Types

import           Geometry

type MDiagram v = Measured Double (Diagram v)

-- | Turn a measured diagram into a 'Diagram' with a 'DelayedLeaf'. The
--   resulting diagram has no 'Envelope' or 'Trace'.
--
--   Units are accesable by the 'Functor' instance of 'Measured':
--
-- @
-- outputCircle = fc blue . circle <$> output 10 :: MDiagram V2
-- @
--
--  Or the 'Monad' instance:
--
-- @
-- topRight :: Diagram V2
-- topRight = measuredDiagram $ do
--   o <- output 10
--   return $ circle o
-- @
--
--  The resulting diagram will have no 'Envelope' or 'Trace'. You can
--  them by either combining this diagram with another that has its own
--  or set them explicitly with the 'withEnvelope' and 'withTrace'
--  combinators.
--
--  The non-scaling part of transforms are applied as normal, but only
--  'local' units get scaled. Other sizes depend on the final size and
--  output of the diagram.
--
--  Note that any names inside a measured diagram cannot be traversed
--  unless the full diagram has had 'unmeasureDiagram' applied.
measuredDiagram :: (HasLinearMap v, OrderedField n, Typeable n, Monoid m)
  => Measured n (QDiagram v n m) -> QDiagram v n m
measuredDiagram md
  = mkQD' (measuredLeaf md)
          mempty -- envelope
          mempty -- trace
          mempty -- query

-- | Turn a measured diagram into a 'DelayedLeaf'.
measuredLeaf :: (HasLinearMap v, OrderedField n, Typeable n, Monoid m)
  => Measured n (QDiagram v n m) -> QDiaLeaf v n m
measuredLeaf md = DelayedLeaf delayedPrim
  where
    delayedPrim da g n =
      applyStyle sty . transform tr' $ unmeasure md (l,g,n)
      where
        tr'       = tr <> scaling (1/l)
        (tr, sty) = untangle da
        l         = avgScale tr

-- | "unmeasure" a diagram by replacing all 'DelayedLeaf's with the
-- diagram calculated from providing it with a normalisation factor
-- depending on the bounding box of the diagram.
--
-- In the following example 'halfCircle' will be half the size of the
-- surrounding diagram (i.e. radius of 1/4 the surrounding diagram) when
-- 'unmeasureDiagram' is applied:
--
-- @
-- halfCircle = measuredDiagram (circle <$> normalised 0.25) # fc dodgerBlue
-- normalCircle = circle 1 # fc orange
-- example =  unmeasureDiagram (halfCircle <> normalCircle)
--        ||| unmeasureDiagram (halfCircle <> (normalCircle # scale 2))
-- @
--
-- Note that a fresh measuredDiagram has no bounding box (because it's
-- size depends on the scaling factors given to it), so using
-- @unmeasureDiagram . measuredDiagram@ will likely result in a division
-- by zero. The diagram should be combined with something that has a
-- concrete size or with the envelope size set with 'withEnvelope'.
unmeasureDiagram
  :: (HasLinearMap v, OrderedField n)
  => QDiagram v n m
  -> QDiagram v n m
unmeasureDiagram d = unmeasureDiagram' 1 n d
  where
    n = normalizedFactor (size d)

-- | "unmeasure" a diagram by replacing all 'DelayedLeaf's with the
--   provided global and normalised scaling factors.
unmeasureDiagram'
  :: (HasLinearMap v, OrderedField n)
  => n -- ^ global scaling factor
  -> n -- ^ normalised scaling factor
  -> QDiagram v n m
  -> QDiagram v n m
unmeasureDiagram' g n = releaf $ \d u -> \case
  DelayedLeaf f -> f d g n
  prim -> down d (mkQDU prim u)


