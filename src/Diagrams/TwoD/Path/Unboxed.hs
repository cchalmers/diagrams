{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Diagrams.TwoD.Path.Unboxed
  ( -- * Constructing path-based diagrams

    uStroke, uStrokeQ
  -- , strokePath, strokeP

    -- * UClipping

  , P.FillRule(..), P._FillRule
  , P.fillRule, P._fillRule

  -- , UClip(..), _UClip, _uclip
  -- , clipBy
  -- , clipTo
  -- , clipped

  ) where


-- import Control.Lens hiding (transform)
-- import Data.Typeable
-- import qualified Data.Foldable as F
-- import Data.Sequence (Seq, singleton)
-- import qualified Data.Vector.Unboxed as U

import Geometry.TwoD.Path
import Geometry.TwoD.Types
import Geometry.Path.Unboxed
-- import Geometry.Transform
import Geometry.TrailLike
-- import Geometry.Envelope
-- import Geometry.Trace
-- import Geometry.Query
-- import Geometry.Space
-- import Geometry.Points
-- import Linear ((^*))

import Diagrams.Types
import qualified Diagrams.TwoD.Path as P

-- | Convert a 'ToPath' object into a diagram.  The resulting diagram has the
--   names 0, 1, ... assigned to each of the path's vertices.
--
--   See also 'stroke'', which takes an extra options record allowing
--   its behaviour to be customized.
--
-- @
-- 'stroke' :: 'UPath' 'V2' 'Double'                  -> 'Diagram' 'V2'
-- 'stroke' :: 'Located' ('UTrail' 'V2' 'Double')       -> 'Diagram' 'V2'
-- 'stroke' :: 'Located' ('UTrail'' 'Loop' 'V2' 'Double') -> 'Diagram' 'V2'
-- 'stroke' :: 'Located' ('UTrail'' 'Line' 'V2' 'Double') -> 'Diagram' 'V2'
-- @
uStroke :: UPath V2 Double -> Diagram V2
uStroke = fmap (Any . (/= 0)) . uStrokeQ

uStrokeQ :: UPath V2 Double -> QDiagram V2 Double Crossings
uStrokeQ = primQD

instance TrailLike (Diagram V2) where
  trailLike = uStroke . trailLike

------------------------------------------------------------------------
-- UClipping
------------------------------------------------------------------------

-- | @UClip@ tracks the accumulated clipping paths applied to a
--   diagram.  Note that the semigroup structure on @UClip@ is list
--   concatenation, so applying multiple clipping paths is sensible.
--   The clipping region is the intersection of all the applied
--   clipping paths.
-- newtype UClip n = UClip (Seq (UPath V2 n)) -- use Sequence?
--   deriving (Typeable, Semigroup)

-- instance Typeable n => AttributeClass (UClip n) where
--   type AttrType (UClip n) = 'TAttr

-- instance AsEmpty (UClip Double) where
--   _Empty = _UClip . _Empty

-- type instance V (UClip n) = V2
-- type instance N (UClip n) = n

-- instance (U.Unbox n, OrderedField n) => Transformable (UClip n) where
--   transform t (UClip ps) = UClip (transform t ps)

-- | A point inside a clip if the point is in 'All' invididual clipping
--   paths.
-- instance (U.Unbox n, RealFloat n) => HasQuery (UClip n) All where
--   getQuery (UClip paths) = Query $ \p ->
--     F.foldMap (All . flip isInsideWinding p) paths

-- _UClip :: Iso' (UClip Double) (Seq (UPath V2 Double))
-- _UClip = coerced

-- | Lens onto the UClip in a style. An empty list means no clipping.
--
-- @
-- '_uclip' :: 'Lens'' ('Style' 'V2' 'Double') ['Path' 'V2' 'Double']
-- @
-- _uclip :: (InSpace V2 Double a, HasStyle a)
--       => Lens' a (Maybe (Seq (UPath V2 Double)))
-- _uclip = style . atAttr _UClip

-- | UClip a diagram by the given path:
--
--   * Only the parts of the diagram which lie in the interior of the
--     path will be drawn.
--
--   * The envelope of the diagram is unaffected.
-- clipBy :: (InSpace V2 Double a, ApplyStyle a) => UPath V2 Double -> a -> a
-- clipBy = applyAttr _UClip . singleton

-- | UClip a diagram to the given path setting its envelope to the
--   pointwise minimum of the envelopes of the diagram and path. The
--   trace consists of those parts of the original diagram's trace
--   which fall within the clipping path, or parts of the path's trace
--   within the original diagram.
-- clipTo :: UPath V2 Double -> Diagram V2 -> Diagram V2
-- clipTo p d = (\t -> modTrace (const t)) . intersectionTrace . toEnvelope $ clipBy p d
--   where
--     envP = appEnvelope . getEnvelope $ p
--     envD = appEnvelope . getEnvelope $ d
--     toEnvelope = case (envP, envD) of
--       (Just eP, Just eD) -> setEnvelope . mkEnvelope $ \v -> min (eP v) (eD v)
--       (_, _)             -> id
--     intersectionTrace = Trace traceIntersections
--     traceIntersections pt v =
--         -- on boundary of d, inside p
--         onSortedList (filter pInside) (appTrace (getTrace d) pt v) <>
--         -- or on boundary of p, inside d
--         onSortedList (filter dInside) (appTrace (getTrace p) pt v) where
--           newPt dist = pt .+^ v ^* dist
--           pInside dDist = isInsideWinding p (newPt dDist)
--           dInside pDist = getAny . sample d $ newPt pDist

-- | UClip a diagram to the clip path taking the envelope and trace of the clip
--   path.
-- clipped :: UPath V2 Double -> Diagram V2 -> Diagram V2
-- clipped p = clipBy p
-- clipped p = withTrace p . withEnvelope p . clipBy p


