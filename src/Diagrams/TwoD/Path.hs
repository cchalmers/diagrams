{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Diagrams.TwoD.Path
  ( -- * Constructing path-based diagrams

    stroke, stroke'
  , strokePath, strokeP, strokePath', strokeP'
  , strokeTrail, strokeT, strokeTrail', strokeT'
  , strokeLine, strokeLoop
  , strokeLocTrail, strokeLocT, strokeLocLine, strokeLocLoop

    -- ** Stroke options

  , FillRule(..), _FillRule
  , fillRule, _fillRule
  , StrokeOpts(..), vertexNames, queryFillRule


    -- * Clipping

  , Clip(..), _Clip, _clip
  , clipBy, clipTo, clipped

  ) where


import Control.Lens hiding (transform)
import Data.Semigroup
import Data.Default.Class
import Data.Typeable
import qualified Data.Foldable as F

import Geometry.TwoD.Path
import Geometry.TwoD.Types
import Geometry.Path
import Geometry.Transform
import Geometry.Envelope
import Geometry.Trace
import Geometry.TrailLike
import Geometry.Trail
import Geometry.Query
import Geometry.Space
import Geometry.Located
-- import Geometry.Points
-- import Linear ((^*))

import Linear.Affine (Point)

import Diagrams.Types

-- | Enumeration of algorithms or \"rules\" for determining which
--   points lie in the interior of a (possibly self-intersecting)
--   path.
data FillRule
  = Winding  -- ^ Interior points are those with a nonzero
             --   /winding/ /number/.  See
             --   <http://en.wikipedia.org/wiki/Nonzero-rule>.
  | EvenOdd  -- ^ Interior points are those where a ray
             --   extended infinitely in a particular direction crosses
             --   the path an odd number of times. See
             --   <http://en.wikipedia.org/wiki/Even-odd_rule>.
    deriving (Show, Typeable, Eq, Ord)
    -- XXX add some pics

instance AttributeClass FillRule where
  type AttrType FillRule = 'IAttr

instance Semigroup FillRule where
  _ <> b = b

instance Default FillRule where
  def = Winding

runFillRule :: FillRule -> Path V2 Double -> Point V2 Double -> Bool
runFillRule Winding = isInsideWinding
runFillRule EvenOdd = isInsideEvenOdd


-- | Identity isomorphism, usefull for type inference.
_FillRule :: Equality' FillRule FillRule
_FillRule = id

-- | Specify the fill rule that should be used for determining which
--   points are inside a path.
fillRule :: ApplyStyle a => FillRule -> a -> a
fillRule = applyAttr _FillRule

-- | Lens onto the fill rule of a style.
_fillRule :: Lens' (Style V2) (Maybe FillRule)
_fillRule = atAttr _FillRule

-- | A record of options that control how a path is stroked.
--   @StrokeOpts@ is an instance of 'Default', so a @StrokeOpts@
--   records can be created using @'with' { ... }@ notation.
data StrokeOpts a = StrokeOpts
  { _vertexNames   :: [[a]]

  , _queryFillRule :: FillRule

  }

makeLensesWith (generateSignatures .~ False $ lensRules) ''StrokeOpts

-- | Atomic names that should be assigned to the vertices of the path so that
--   they can be referenced later.  If there are not enough names, the extra
--   vertices are not assigned names; if there are too many, the extra names
--   are ignored.  Note that this is a /list of lists/ of names, since paths
--   can consist of multiple trails.  The first list of names are assigned to
--   the vertices of the first trail, the second list to the second trail, and
--   so on.
--
--   The default value is the empty list.

vertexNames :: Lens (StrokeOpts a) (StrokeOpts a') [[a]] [[a']]

-- | The fill rule used for determining which points are inside the path.
--   The default is 'Winding'.  NOTE: for now, this only affects the resulting
--   diagram's 'Query', /not/ how it will be drawn!  To set the fill rule
--   determining how it is to be drawn, use the 'fillRule' function.
queryFillRule :: Lens' (StrokeOpts a) FillRule

instance Default (StrokeOpts a) where
  def = StrokeOpts
   { _vertexNames    = []
   , _queryFillRule = def
   }

-- | Convert a 'ToPath' object into a diagram.  The resulting diagram has the
--   names 0, 1, ... assigned to each of the path's vertices.
--
--   See also 'stroke'', which takes an extra options record allowing
--   its behaviour to be customized.
--
-- @
-- 'stroke' :: 'Path' 'V2' 'Double'                  -> 'Diagram' 'V2'
-- 'stroke' :: 'Located' ('Trail' 'V2' 'Double')       -> 'Diagram' 'V2'
-- 'stroke' :: 'Located' ('Trail'' 'Loop' 'V2' 'Double') -> 'Diagram' 'V2'
-- 'stroke' :: 'Located' ('Trail'' 'Line' 'V2' 'Double') -> 'Diagram' 'V2'
-- @
stroke :: (InSpace V2 Double t, ToPath t)
       => t -> Diagram V2
stroke = strokeP . toPath

-- | A variant of 'stroke' that takes an extra record of options to
--   customize its behaviour.  In particular:
--
--     * Names can be assigned to the path's vertices
--
--   'StrokeOpts' is an instance of 'Default', so @stroke' ('with' &
--   ... )@ syntax may be used.
stroke' :: (InSpace V2 Double t, ToPath t, IsName a)
       => StrokeOpts a -> t -> Diagram V2
stroke' opts = strokeP' opts . toPath

-- | 'stroke' specialised to 'Path'.
strokeP :: Path V2 Double -> Diagram V2
strokeP = strokeP' (def :: StrokeOpts ())

-- | 'stroke' specialised to 'Path'.
strokePath :: Path V2 Double -> Diagram V2
strokePath = strokeP

-- | 'stroke'' specialised to 'Path'.
strokeP' :: IsName a => StrokeOpts a -> Path V2 Double -> Diagram V2
strokeP' opts path
  | null (pLines ^. _Wrapped') = mkP pLoops
  | null (pLoops ^. _Wrapped') = mkP pLines
  | otherwise                  = mkP pLines <> mkP pLoops
  where
    (pLines,pLoops) = partitionPath (isLine . unLoc) path
    _xxx = toName (opts^.vertexNames)
    -- _xxx = (fromNames . concat $
    --         zipWith zip (opts^.vertexNames) ((map . map) subPoint (pathVertices p)))
    mkP p
      = mkQD (Prim p)
         (getEnvelope p)
         (getTrace p)
         (Query $ Any . (runFillRule (opts^.queryFillRule)) p)

-- | 'stroke'' specialised to 'Path'.
strokePath' :: IsName a => StrokeOpts a -> Path V2 Double -> Diagram V2
strokePath' = strokeP'

-- | 'stroke' specialised to 'Trail'.
strokeTrail :: Trail V2 Double -> Diagram V2
strokeTrail = stroke . pathFromTrail

-- | 'stroke' specialised to 'Trail'.
strokeT :: Trail V2 Double -> Diagram V2
strokeT = strokeTrail

-- | A composition of 'stroke'' and 'pathFromTrail' for conveniently
--   converting a trail directly into a diagram.
strokeTrail' :: (IsName a) => StrokeOpts a -> Trail V2 Double -> Diagram V2
strokeTrail' opts = stroke' opts . pathFromTrail

-- | Deprecated synonym for 'strokeTrail''.
strokeT' :: IsName a => StrokeOpts a -> Trail V2 Double -> Diagram V2
strokeT' = strokeTrail'

-- | A composition of 'strokeT' and 'wrapLine' for conveniently
--   converting a line directly into a diagram.
strokeLine :: Trail' Line V2 Double -> Diagram V2
strokeLine = strokeT . wrapLine

-- | A composition of 'strokeT' and 'wrapLoop' for conveniently
--   converting a loop directly into a diagram.
strokeLoop :: Trail' Loop V2 Double -> Diagram V2
strokeLoop = strokeT . wrapLoop

-- | A convenience function for converting a @Located Trail@ directly
--   into a diagram; @strokeLocTrail = stroke . trailLike@.
strokeLocTrail :: Located (Trail V2 Double) -> Diagram V2
strokeLocTrail = strokeP . trailLike

-- | Deprecated synonym for 'strokeLocTrail'.
strokeLocT :: Located (Trail V2 Double) -> Diagram V2
strokeLocT = strokeLocTrail

-- | A convenience function for converting a @Located@ line directly
--   into a diagram; @strokeLocLine = stroke . trailLike . mapLoc wrapLine@.
strokeLocLine :: Located (Trail' Line V2 Double) -> Diagram V2
strokeLocLine = strokeP . trailLike . mapLoc wrapLine

-- | A convenience function for converting a @Located@ loop directly
--   into a diagram; @strokeLocLoop = stroke . trailLike . mapLoc wrapLoop@.
strokeLocLoop :: Located (Trail' Loop V2 Double) -> Diagram V2
strokeLocLoop = strokeP . trailLike . mapLoc wrapLoop

------------------------------------------------------------------------
-- Clipping
------------------------------------------------------------------------

-- | @Clip@ tracks the accumulated clipping paths applied to a
--   diagram.  Note that the semigroup structure on @Clip@ is list
--   concatenation, so applying multiple clipping paths is sensible.
--   The clipping region is the intersection of all the applied
--   clipping paths.
newtype Clip = Clip [Path V2 Double] -- use Sequence?
  deriving (Typeable, Semigroup)

instance AttributeClass Clip where
  type AttrType Clip = 'TAttr

instance AsEmpty Clip where
  _Empty = _Clip . _Empty

type instance V Clip = V2
type instance N Clip = Double

instance Transformable Clip where
  transform t (Clip ps) = Clip (transform t ps)

-- | A point inside a clip if the point is in 'All' invididual clipping
--   paths.
instance HasQuery Clip All where
  getQuery (Clip paths) = Query $ \p ->
    F.foldMap (All . flip isInsideWinding p) paths

_Clip :: Iso' Clip [Path V2 Double]
_Clip = coerced

-- | Lens onto the Clip in a style. An empty list means no clipping.
--
-- @
-- '_clip' :: 'Lens'' ('Style' 'V2' 'Double') ['Path' 'V2' 'Double']
-- @
_clip :: (InSpace V2 Double a, HasStyle a)
      => Lens' a (Maybe [Path V2 Double])
_clip = style . atAttr _Clip

-- | Clip a diagram by the given path:
--
--   * Only the parts of the diagram which lie in the interior of the
--     path will be drawn.
--
--   * The envelope of the diagram is unaffected.
clipBy :: (InSpace V2 Double a, ApplyStyle a) => Path V2 Double -> a -> a
clipBy = applyAttr _Clip . (:[])

-- | Clip a diagram to the given path setting its envelope to the
--   pointwise minimum of the envelopes of the diagram and path. The
--   trace consists of those parts of the original diagram's trace
--   which fall within the clipping path, or parts of the path's trace
--   within the original diagram.
clipTo :: Path V2 Double -> Diagram V2 -> Diagram V2
clipTo = clipped -- p d = setTrace intersectionTrace . toEnvelope $ clipBy p d
  -- where
  --   envP = appEnvelope . getEnvelope $ p
  --   envD = appEnvelope . getEnvelope $ d
  --   toEnvelope = case (envP, envD) of
  --     (Just eP, Just eD) -> setEnvelope . mkEnvelope $ \v -> min (eP v) (eD v)
  --     (_, _)             -> id
  --   intersectionTrace = Trace traceIntersections
  --   traceIntersections pt v =
  --       -- on boundary of d, inside p
  --       onSortedList (filter pInside) (appTrace (getTrace d) pt v) <>
  --       -- or on boundary of p, inside d
  --       onSortedList (filter dInside) (appTrace (getTrace p) pt v) where
  --         newPt dist = pt .+^ v ^* dist
  --         pInside dDist = runFillRule Winding p (newPt dDist)
  --         dInside pDist = getAny . sample d $ newPt pDist

-- | Clip a diagram to the clip path taking the envelope and trace of the clip
--   path.
clipped :: Path V2 Double -> Diagram V2 -> Diagram V2
clipped p = clipBy p
-- clipped p = withTrace p . withEnvelope p . clipBy p

