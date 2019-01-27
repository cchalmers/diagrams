{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Combinators
-- Copyright   :  (c) 2011-2018 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Connecting (possibly named) diagrams with arrows.
--
-----------------------------------------------------------------------------

module Diagrams.Arrow where
  -- ( connect
  -- , connectOutside
  -- ) where

import           Control.Lens                       hiding (beside)

import           Geometry
import           Linear
import           Linear.Vector                      ((*^))

import           Data.Colour.Names
import           Data.Maybe
import           Data.Typeable
import           Diagrams.Attributes
import           Diagrams.Measured
import           Diagrams.TwoD.Attributes
import           Diagrams.Types

-- | Given a direction and a length, return an arrow head/tail.
data Arrow v = Arrow
  {  pointArrow :: Direction v Double -> Double -> Diagram v
  }

data ArrowOpts v = ArrowOpts
  { _arrowShaft :: Trail v Double
  , _shaftStyle :: Style v Double
  , _arrowHead  :: Arrow v
  , _headGap    :: Measure Double
  , _headLength :: Measure Double
  , _headStyle  :: Style v Double
  , _arrowTail  :: Arrow v
  , _tailGap    :: Measure Double
  , _tailStyle  :: Style v Double
  , _tailLength :: Measure Double
  }

makeLenses 'ArrowOpts

-- | Funky arrow opts for testing.
myArrowOpts :: ArrowOpts V2
myArrowOpts = ArrowOpts
  { _arrowShaft = origin ~~ unitX
  , _shaftStyle = mempty & lc brown
  , _arrowHead  = Arrow $ \d n -> scaleUToX n (triangle 0.5 & rotateBy (1/12) & alignR) & rotateTo d
  , _headGap    = 10
  , _headLength = 60
  , _headStyle  = mempty & fc orange
  , _arrowTail  = Arrow $ \d n -> scaleUToX n (triangle 0.5 & rotateBy (1/12) & reflectX & alignL) & rotateTo d
  , _tailGap    = 10
  , _tailStyle  = mempty & fc brown
  , _tailLength = local 0.2
  }

toMaybe :: Foldable f => f a -> Maybe a
toMaybe = preview folded

-- | Stroke without an envelope, trace or query.
strokeEmpty :: Typeable v => Path v Double -> Diagram v
strokeEmpty p = mkQD (Prim p) mempty mempty mempty

-- | Connect two names with a straight line.
connect
  :: (Typeable v, HasLinearMap v, IsName n1, IsName n2)
  => n1
  -> n2
  -> Diagram v
  -> Diagram v
connect n1 n2 d =
  case ps of
    Just (pa,pb) -> strokeEmpty $ pa ~~ pb
    Nothing      -> mempty
  where
    ps = do
      pa <- toMaybe $ findSubs n1 d
      pb <- toMaybe $ findSubs n2 d
      Just (subLocation pa, subLocation pb)

-- | Connect two named things with a stright line between their perimeters.
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

-- | Connect two named things using the arrow options. For now they are
--   connected using a straight line, in the future this will use the
--   'arrowShaft'.
connectOutside'
  :: (IsName n1, IsName n2)
  => ArrowOpts V2
  -> n1
  -> n2
  -> Diagram V2
  -> Diagram V2
connectOutside' opts n1 n2 dia =
  case ps of
    Nothing      -> mempty
    Just (pa0,pb0) -> measuredDiagram $ do
      -- the local unit to get things in terms of local space
      l          <- local 1
      hLength <- opts ^. headLength
      tLength <- opts ^. tailLength
      hgap <- opts ^. headGap
      tgap <- opts ^. tailGap
      let -- the points in the correct scale
          (pa1,pb1) = (pa0,pb0) & scale l
          -- the direction between the points
          d = dirBetween pa1 pb1
          -- the points after we account for the gaps
          (pa,pb) = (pa1 .+^ tgap *^ fromDir d, pb1 .-^ hgap *^ fromDir d)
          -- the points after we account for the arrow sizes
          pa' = pa .+^ (tLength *^ fromDir d)
          pb' = pb .-^ (hLength *^ fromDir d)
          -- the shaft between the two points (straight line only for now)
          shaft = pa' ~~ pb' & applyStyle (opts ^. shaftStyle)
          ahead  = moveTo pb
                 $ pointArrow (opts ^. arrowHead) d hLength & applyStyle (opts ^. headStyle) & opacity 0.5
          atail  = moveTo pa
                 $ pointArrow (opts ^. arrowTail) d tLength & applyStyle (opts ^. tailStyle) & opacity 0.5
      pure $ ahead <> atail <> shaft
  where
    ps = do
      sa <- toMaybe $ findSubs n1 dia
      sb <- toMaybe $ findSubs n2 dia
      let pa = subLocation sa
          pb = subLocation sb
          v  = pb .-. pa
          m  = pa .+^ v^/2
          da = getSub sa
          db = getSub sb
          ta = fromMaybe pa $ traceP m (negated v) da
          tb = fromMaybe pa $ traceP m v db
      Just (ta,tb)
