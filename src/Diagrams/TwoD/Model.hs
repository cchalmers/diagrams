{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Model
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Tools for visualizing diagrams' internal model: local origins,
-- envelopes, traces, /etc./
--
-----------------------------------------------------------------------------
module Diagrams.TwoD.Model
  ( -- * Showing the local origin
    showOrigin
  , showOrigin'
  , OriginOpts(..), oColor, oScale, oMinSize

    -- * Showing an approximation of the envelope
  , showEnvelope
  , showEnvelope'
  , EnvelopeOpts(..), eColor, eLineWidth, ePoints

    -- * Showing an approximation of the trace
  , showTrace
  , showTrace'
  , TraceOpts(..), tColor, tScale, tMinSize, tPoints

    -- * Showing labels of all named subdiagrams
  , showLabels
  ) where

import           Control.Arrow            (second)
import           Control.Lens             (makeLenses, (^.))
import           Data.Colour              (Colour)
import           Data.Colour.Names
import           Data.Default.Class
import           Data.List                (intercalate)
import qualified Data.Map                 as M
import           Data.Maybe               (catMaybes)
import           Data.Semigroup
import Geometry

import           Diagrams.Attributes
import           Diagrams.Combinators     (atPoints)
import           Diagrams.Names
import           Diagrams.CubicSpline
import           Diagrams.Path
import           Diagrams.TwoD.Attributes
import           Diagrams.TwoD.Ellipse
import           Diagrams.TwoD.Path
import           Diagrams.TwoD.Text
import           Diagrams.TwoD.Transform  (rotateBy)
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector     (unitX)
import           Diagrams.Util

import           Linear.Affine
import           Linear.Vector

------------------------------------------------------------------------
-- Marking the origin
------------------------------------------------------------------------

-- | Options for displaying the origin.
data OriginOpts n = OriginOpts (Style V2 n) (Measure n)

type instance V (OriginOpts n) = V2
type instance N (OriginOpts n) = n

-- | The size of the displayed origin.
originSize :: Lens' (OriginOpts n) (Measure n)
originSize f (OriginOpts sty sz) = OriginOpts sty <$> f sz

instance Fractional n => Default (OriginOpts n) where
  def = OriginOpts (mempty # fc red # lw none) (normalized $ 1/50)

instance ApplyStyle (OriginOpts n) where
  applyStyle = over style

instance HasStyle (OriginOpts n) where
  style f (OriginOpts sty sz) = f sty <&> \sty' -> OriginOpts sty' sz

-- | Mark the origin of a diagram by placing a red dot 1/50th its size.
showOrigin :: (TypeableFloat n, Renderable (Path V2 n) b, Monoid' m)
           => QDiagram b V2 n m -> QDiagram b V2 n m
showOrigin = showOrigin' def

-- | Mark the origin of a diagram, with control over colour and scale
-- of marker dot.
showOrigin' :: (TypeableFloat n, Renderable (Path V2 n) b, Monoid' m)
           => OriginOpts n -> QDiagram b V2 n m -> QDiagram b V2 n m
showOrigin' oo d = o <> d
  where
    o = strokeP (circle sz)
          # fc (oo^.oColor)
          # lw none
          # fmap (const mempty)
    V2 w h = oo^.oScale *^ size d
    sz     = maximum [w, h, oo^.oMinSize]

------------------------------------------------------------------------
-- Approximating the envelope
------------------------------------------------------------------------

-- | Options for displaying the envelope approximation.
data EnvelopeOpts n = EnvelopeOpts (Style V2 n) Bool Int

instance OrderedField n => Default (EnvelopeOpts n) where
  def = EnvelopeOpts (mempty & lc ref & lw medium) 32

instance ApplyStyle (OriginOpts n) where
  applyStyle = over style

instance HasStyle (OriginOpts n) where
  style f (EnvelopeOpts sty s sz) = f sty <&> \sty' -> EnvelopeOpts sty' s sz

-- | Number of points used to estimate the envelope.
--
--   Default is @32@.
envelopePoints :: Lens' (EnvelopeOpts n) Int
envelopePoints f (EnvelopeOpts sty s n) = EnvelopeOpts sty s <$> f n

-- | Should the resulting envelope be smoothed.
--
--   Default is 'True'.
envelopeSmoothing :: Lens' (EnvelopeOpts n) Bool
envelopeSmoothing f (EnvelopeOpts sty s n) =  f s <&> \s' -> EnvelopeOpts sty s' sz

-- | Mark the envelope with an approximating cubic spline with control
--   over the color, line width and number of points.
showEnvelope'
  :: TypeableFloat n
  => EnvelopeOpts n -> QDiagram V2 n Any -> QDiagram V2 n Any
showEnvelope' opts d = cubicSpline True pts # lc (opts^.eColor)
                                            # lw w <> d
  where
    pts = catMaybes [envelopePMay v d | v <- map (`rotateBy` unitX) [0,inc..top]]
    w   = opts ^. eLineWidth
    inc = 1 / fromIntegral (opts^.ePoints)
    top = 1 - inc


-- | Mark the envelope with an approximating cubic spline
--   using 32 points, medium line width and red line color.
showEnvelope :: TypeableFloat n
             => QDiagram V2 n Any -> QDiagram V2 n Any
showEnvelope = showEnvelope' def

------------------------------------------------------------------------
-- Approximating the trace
------------------------------------------------------------------------

-- | Options for displaying the trace appromiation.
data TraceOpts n = TraceOpts
  { _tColor   :: Colour Double
  , _tScale   :: n
  , _tMinSize :: n
  , _tPoints  :: Int
  }

makeLenses ''TraceOpts

-- | Options for displaying the envelope approximation.
data TraceOpts n = TraceOpts (Style V2 n) (Measure n) Int

instance OrderedField n => Default (TraceOpts n) where
  def = TraceOpts (mempty & lc ref & lw medium) 32

instance ApplyStyle (OriginOpts n) where
  applyStyle = over style

instance HasStyle (OriginOpts n) where
  style f (TraceOpts sty s sz) = f sty <&> \sty' -> TraceOpts sty' s sz

-- | Number of points used to estimate the trace.
--
--   Default is @64@.
tracePoints :: Lens' (TraceOpts n) Int
tracePoints f (TraceOpts sty s n) = TraceOpts sty s <$> f n

-- | Should the resulting trace be smoothed.
--
--   Default is 'True'.
traceSmoothing :: Lens' (TraceOpts n) Bool
traceSmoothing f (TraceOpts sty s n) =  f s <&> \s' -> TraceOpts sty s' sz


instance Floating n => Default (TraceOpts n) where
  def = TraceOpts red (1/100) 0.001 64

-- | Mark the trace of a diagram, with control over colour and scale
-- of marker dot and the number of points on the trace.
showTrace'
  :: TypeableFloat n
  => TraceOpts n -> QDiagram V2 n Any -> QDiagram V2 n Any
showTrace' opts d =  atPoints ps (repeat pt) <> d
  where
    ps = concatMap p ts
    ts = zip rs vs
    p (r, v) = [origin .+^ (s *^ v) | s <- r]
    vs = map (`rotateBy` unitX) [0, inc..top]
    rs = [getSortedList $ (appTrace . getTrace) d origin v | v <- vs]
    pt = circle sz # fc (opts^.tColor) # lw none
    V2 w h = opts^.tScale *^ size d
    sz     = maximum [w, h, opts^.tMinSize]
    inc = 1 / fromIntegral (opts^.tPoints)
    top = 1 - inc

-- | Mark the trace of a diagram by placing 64 red dots 1/100th its size
--   along the trace.
showTrace
  :: TypeableFloat n
  => QDiagram V2 n Any -> QDiagram V2 n Any
showTrace = showTrace' def

------------------------------------------------------------------------
-- Labeling named points
------------------------------------------------------------------------

-- showLabels :: (TypeableFloat n, Renderable (Text n) b, Semigroup m)
--            => QDiagram b V2 n m -> QDiagram b V2 n Any
-- showLabels d =
--              ( mconcat
--              . map (\(n,p) -> text (simpleName n) # translate (p .-. origin))
--              . concatMap (\(n,ps) -> zip (repeat n) ps)
--              . (map . second . map) location
--              . M.assocs
--              $ m
--              ) <>
--              fmap (const (Any False)) d
--   where
--     SubMap m = d^.subMap
--     simpleName (Name ns) = intercalate " .> " $ map simpleAName ns
--     simpleAName (AName n) = show n
