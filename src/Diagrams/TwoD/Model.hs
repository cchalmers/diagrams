{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Model
-- Copyright   :  (c) 2016 diagrams team (see LICENSE)
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
  , OriginOpts(..), originSize -- , oMinSize

    -- * Showing an approximation of the envelope
  -- , showEnvelope
  -- , showEnvelope'
  , EnvelopeOpts(..), envelopePoints, envelopeSmoothing

    -- * Showing an approximation of the trace
  , showTrace
  , showTrace'
  , TraceOpts(..), tracePoints, traceSmoothing

    -- * Showing labels of all named subdiagrams
  , showLabels
  ) where

import           Control.Lens              hiding (none, ( # ))
import           Data.Colour.Names
import           Data.Default.Class
import           Data.Foldable
import           Data.List                 (intercalate)
import           Data.Semigroup
import           Linear.Vector

import           Data.Monoid.WithSemigroup
import           Diagrams.Attributes
import           Diagrams.Combinators      (atPoints)
import           Diagrams.TwoD.Attributes
import           Diagrams.TwoD.Text
import           Diagrams.Types
import           Diagrams.Measured
import           Diagrams.Util
import           Geometry

------------------------------------------------------------------------
-- Marking the origin
------------------------------------------------------------------------

-- | Options for displaying the origin.
data OriginOpts = OriginOpts (Style V2 Double) (Measure Double)

type instance V OriginOpts = V2
type instance N OriginOpts = Double

-- | The size of the displayed origin.
originSize :: Lens' OriginOpts (Measure Double)
originSize f (OriginOpts sty sz) = OriginOpts sty <$> f sz

instance Default OriginOpts where
  def = OriginOpts (mempty # fc red # lw none) (normalized $ 1/50)

instance ApplyStyle OriginOpts where
  applyStyle s = style <>~ s

instance HasStyle OriginOpts where
  style f (OriginOpts sty sz) = f sty <&> \sty' -> OriginOpts sty' sz

-- | Mark the origin of a diagram by placing a red dot 1/50th its size.
showOrigin :: Monoid' m => QDiagram V2 Double m -> QDiagram V2 Double m
showOrigin = showOrigin' def

-- | Mark the origin of a diagram, with control over colour and scale
-- of marker dot.
showOrigin'
  :: Monoid' m
  => OriginOpts -> QDiagram V2 Double m -> QDiagram V2 Double m
showOrigin' (OriginOpts sty m) d = measuredDiagram o <> d
  where
    o = m <&> \sz -> strokePath (circle sz)
          # applyStyle sty
          # lw none
          # fmap (const mempty)

------------------------------------------------------------------------
-- Approximating the envelope
------------------------------------------------------------------------

-- | Options for displaying the envelope approximation.
data EnvelopeOpts = EnvelopeOpts (Style V2 Double) Bool Int

type instance V EnvelopeOpts = V2
type instance N EnvelopeOpts = Double

instance Default EnvelopeOpts where
  def = EnvelopeOpts (mempty & lc red & lw medium) True 32

instance ApplyStyle EnvelopeOpts where
  applyStyle s = style <>~ s

instance HasStyle EnvelopeOpts where
  style f (EnvelopeOpts sty s sz) = f sty <&> \sty' -> EnvelopeOpts sty' s sz

-- | Number of points used to estimate the envelope.
--
--   Default is @32@.
envelopePoints :: Lens' EnvelopeOpts Int
envelopePoints f (EnvelopeOpts sty s n) = EnvelopeOpts sty s <$> f n

-- | Should the resulting envelope be smoothed.
--
--   Default is 'True'.
envelopeSmoothing :: Lens' EnvelopeOpts Bool
envelopeSmoothing f (EnvelopeOpts sty s n) =  f s <&> \s' -> EnvelopeOpts sty s' n

-- | Mark the envelope with an approximating cubic spline with control
--   over the color, line width and number of points.
-- showEnvelope' :: EnvelopeOpts -> Diagram V2 -> Diagram V2
-- showEnvelope' opts d = fromVertices pts # applyStyle (opts^.style) <> d
--   where
--     pts = catMaybes [envelopePMay v d | v <- map (`rotateBy` unitX) [0,inc..top]]
--     -- w   = opts ^. eLineWidth
--     inc = 1 / fromIntegral (opts^.envelopePoints)
--     top = 1 - inc


-- | Mark the envelope with an approximating cubic spline
--   using 32 points, medium line width and red line color.
-- showEnvelope :: Diagram V2 -> Diagram V2
-- showEnvelope = showEnvelope' def

------------------------------------------------------------------------
-- Approximating the trace
------------------------------------------------------------------------

-- | Options for displaying the trace appromiation.
-- data TraceOpts n = TraceOpts
--   { _tColor   :: Colour Double
--   , _tScale   :: n
--   , _tMinSize :: n
--   , _tPoints  :: Int
--   }

-- makeLenses ''TraceOpts

-- | Options for displaying the envelope approximation.
data TraceOpts = TraceOpts (Style V2 Double) Bool Int

type instance V TraceOpts = V2
type instance N TraceOpts = Double

instance Default TraceOpts where
  def = TraceOpts (mempty & lc red & lw medium) True 32

instance ApplyStyle TraceOpts where
  applyStyle s = style <>~ s

instance HasStyle TraceOpts where
  style f (TraceOpts sty s sz) = f sty <&> \sty' -> TraceOpts sty' s sz

-- | Number of points used to estimate the trace.
--
--   Default is @64@.
tracePoints :: Lens' TraceOpts Int
tracePoints f (TraceOpts sty s n) = TraceOpts sty s <$> f n

-- | Should the resulting trace be smoothed.
--
--   Default is 'True'.
traceSmoothing :: Lens' TraceOpts Bool
traceSmoothing f (TraceOpts sty s n) =  f s <&> \s' -> TraceOpts sty s' n

-- instance Floating n => Default (TraceOpts n) where
--   def = TraceOpts red (1/100) 0.001 64

-- | Mark the trace of a diagram, with control over colour and scale
-- of marker dot and the number of points on the trace.
showTrace' :: TraceOpts -> Diagram V2 -> Diagram V2
showTrace' opts d =  atPoints ps (repeat pt) <> d
  where
    ps = concatMap p ts
    ts = zip rs vs
    p (r, v) = [origin .+^ (s *^ v) | s <- r]
    vs = map (`rotateBy` unitX) [0, inc..top]
    -- rs = [getSortedList $ (appTrace . getTrace) d origin v | v <- vs]
    rs = map (toList . (appTrace . getTrace) d origin) vs
    pt = circle sz # applyStyle (opts^.style)
    V2 w h = undefined -- opts^.tScale *^ size d
    sz     = maximum [w, h] -- , opts^.tMinSize]
    inc = 1 / fromIntegral (opts^.tracePoints)
    top = 1 - inc

-- | Mark the trace of a diagram by placing 64 red dots 1/100th its size
--   along the trace.
showTrace :: Diagram V2 -> Diagram V2
showTrace = showTrace' def

------------------------------------------------------------------------
-- Labeling named points
------------------------------------------------------------------------

mkLabels
  :: (TypeableFloat n, Monoid' m)
  => (String -> QDiagram V2 n m) -> QDiagram V2 n m -> QDiagram V2 n m
mkLabels f d = foldMap mkLabel (allSubs d) where
  mkLabel (nm, sub) = f (prettyName nm) # moveTo (subLocation sub)

-- | Show the labels in a diagram.
showLabels :: Diagram V2 -> Diagram V2
showLabels d = mkLabels text d <> d

-- | Display a name without the \"toName\" prefix for singular names.
prettyName :: Name -> String
prettyName (Name ns) = intercalate " .> " $ map (\(AName n) -> show n) ns
