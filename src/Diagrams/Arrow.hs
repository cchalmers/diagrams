{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Combinators
-- Copyright   :  (c) 2011-2016 diagrams-core team (see LICENSE)
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

module Diagrams.Arrow where
  -- ( connect
  -- , connectOutside
  -- ) where

import           Control.Lens                       hiding (beside)
import           Data.Monoid.WithSemigroup

import           Geometry
import           Linear
import           Linear.Metric
import           Linear.Vector                      ((*^))

import           Diagrams.Types
import qualified Numeric.Interval.NonEmpty.Internal as I
import Data.Maybe
import Data.Typeable

data ArrowOpts v = ArrowOpts
  { _arrowShaft :: Trail v Double
  , _shaftStyle :: Style v Double
  , _arrowHead  :: Diagram v
  , _headGap    :: Measure Double
  , _headLength :: Measure Double
  , _headStyle  :: Style v Double
  , _arrowTail  :: Diagram v
  , _tailGap    :: Measure Double
  , _tailStyle  :: Style v Double
  , _tailLength :: Measure Double
  }

toMaybe :: Foldable f => f a -> Maybe a
toMaybe = preview folded

-- | Stroke without an envelope, trace or query.
strokeEmpty :: Typeable v => Path v Double -> Diagram v
strokeEmpty p = mkQD (Prim p) mempty mempty mempty

connectWith
  :: (Typeable v, HasLinearMap v, IsName n1, IsName n2)
  => ArrowOpts v
  -> n1
  -> n2
  -> Diagram v
  -> Diagram v
connectWith opts n1 n2 d =
  case ps of
    Just (pa,pb) -> strokeEmpty $ pa ~~ pb
    Nothing      -> mempty
  where
    ps = do
      pa <- toMaybe $ findSubs n1 d
      pb <- toMaybe $ findSubs n2 d
      Just (subLocation pa, subLocation pb)

connectOutside
  :: (IsName n1, IsName n2)
  => n1
  -> n2
  -> Diagram V2
  -> Diagram V2
connectOutside n1 n2 d =
  case ps of
    Just (pa,pb) -> pa ~~ pb
    Nothing      -> mempty
  where
    ps = do
      sa <- toMaybe $ findSubs n1 d
      sb <- toMaybe $ findSubs n2 d
      let pa = subLocation sa
          pb = subLocation sb
          v  = pb .-. pa
          m  = pa .+^ v^/2
          da = getSub sa
          db = getSub sb
          ta = fromMaybe pa $ traceP m (negated v) da
          tb = fromMaybe pa $ traceP m v db
      Just (ta,tb)



--       Just (subLocation pa, subLocation pb)
-- connectOutside'
--   :: (TypeableFloat n, Renderable (Path V2 n) b, IsName n1, IsName n2)
--   => ArrowOpts n -> n1 -> n2 -> QDiagram b V2 n Any -> QDiagram b V2 n Any
-- connectOutside' opts n1 n2 =
--   withName n1 $ \b1 ->
--   withName n2 $ \b2 ->
--     let v = location b2 .-. location b1
--         midpoint = location b1 .+^ (v ^/ 2)
--         s' = fromMaybe (location b1) $ traceP midpoint (negated v) b1
--         e' = fromMaybe (location b2) $ traceP midpoint v b2
--     in
--       atop (arrowBetween' opts s' e')


