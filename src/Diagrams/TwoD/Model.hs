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
  , OriginOpts(..)
  , originSize

    -- * Showing an approximation of the envelope
  , showEnvelope
  , showEnvelope'
  , EnvelopeOpts(..)
  , envelopeNumPoints
  , envelopeSmoothing

    -- * Showing an approximation of the trace
  , showTrace
  , showTrace'
  , TraceOpts(..)
  , traceNumPoints
  , traceMarker

    -- * Showing labels of all named subdiagrams
  , showLabels
  ) where

import           Control.Lens              hiding (none, ( # ))
import           Data.Colour.Names
import           Data.Default.Class
import           Data.List                 (intercalate)
import           Linear.Vector
import           Numeric.Interval.NonEmpty.Internal


import           Data.Monoid.WithSemigroup
import           Diagrams.Attributes
import           Diagrams.TwoD.Attributes
import           Diagrams.TwoD.Text
import           Diagrams.TwoD.Path
import           Diagrams.Types
import           Diagrams.Measured
import           Diagrams.Util
import           Geometry
import           Geometry.Envelope
import           Geometry.Direction
import           Geometry.Trace

------------------------------------------------------------------------
-- Marking the origin
------------------------------------------------------------------------

-- | Options for displaying the origin.
data OriginOpts = OriginOpts
  { _originStyle :: Style V2 Double
  , _originName  :: Name
  , _originSize  :: Measure Double
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''OriginOpts

type instance V OriginOpts = V2
type instance N OriginOpts = Double

originStyle :: Lens' OriginOpts (Style V2 Double)
originName  :: Lens' OriginOpts Name

-- | The size of the displayed origin.
originSize :: Lens' OriginOpts (Measure Double)

instance Default OriginOpts where
  def = OriginOpts
    { _originStyle = mempty # fc red # lw none
    , _originName  = mempty
    , _originSize  = normalized $ 1/50
    }

instance ApplyStyle OriginOpts where
  applyStyle s = style <>~ s

instance HasStyle OriginOpts where
  style = originStyle

instance Qualifiable OriginOpts
instance HasName OriginOpts where
  name = originName

-- | Mark the origin of a diagram by placing a red dot 1/50th its size.
showOrigin :: Monoid m => QDiagram V2 Double m -> QDiagram V2 Double m
showOrigin = showOrigin' def

-- | Mark the origin of a diagram, with control over colour and scale
-- of marker dot.
showOrigin'
  :: Monoid m
  => OriginOpts -> QDiagram V2 Double m -> QDiagram V2 Double m
showOrigin' opts d = (opts^.name .>> measuredDiagram o) <> d
  where
    o = opts^.originSize <&> \sz -> strokePath (circle sz)
          # applyStyle (opts^.style)
          # lw none
          # fmap (const mempty)

------------------------------------------------------------------------
-- Approximating the envelope
------------------------------------------------------------------------

-- | Options for displaying the envelope approximation.
data EnvelopeOpts = EnvelopeOpts
  { _envStyle          :: Style V2 Double
  , _envName           :: Name
  , _envelopeSmoothing :: Bool
  , _envelopeNumPoints :: Int
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''EnvelopeOpts

envStyle :: Lens' EnvelopeOpts (Style V2 Double)
envName  :: Lens' EnvelopeOpts Name

-- | Number of points used to estimate the envelope.
--
--   Default is @32@.
envelopeNumPoints :: Lens' EnvelopeOpts Int

-- | Should the resulting envelope be smoothed.
--
--   Default is 'True'.
envelopeSmoothing :: Lens' EnvelopeOpts Bool

type instance V EnvelopeOpts = V2
type instance N EnvelopeOpts = Double

instance Default EnvelopeOpts where
  def = EnvelopeOpts
    { _envStyle = mempty & lc red & lw medium
    , _envName  = mempty
    , _envelopeSmoothing = True
    , _envelopeNumPoints = 64
    }

instance ApplyStyle EnvelopeOpts where
  applyStyle s = style <>~ s

instance HasStyle EnvelopeOpts where
  style = envStyle

instance Qualifiable EnvelopeOpts
instance HasName EnvelopeOpts where
  name = envName

strokeLocLoop :: Located (Loop V2 Double) -> Diagram V2
strokeLocLoop = stroke

-- Create N unit vectors, forming half the spokes of a wheel
sampleVectors :: Int -> [V2 Double]
sampleVectors n = map (\i -> angleV ((fromIntegral i / n') @@ turn)) [0..n - 1]
  where
    n'  = fromIntegral (2*n)

-- | Mark the envelope with an approximating cubic spline with control
--   over the color, line width and number of points.
showEnvelope' :: EnvelopeOpts -> Diagram V2 -> Diagram V2
showEnvelope' opts d = (opts^.name .>> draw pts # applyStyle (opts^.style)) <> d
  where
    draw
      | opts^.envelopeSmoothing = cubicSpline True
      | otherwise               = strokeLocLoop . fromVertices
    pts = case getEnvelope d of
            Envelope f ->
              let g v = let I a b = f (Dir v)
                        in  (P (a *^ v), P (b *^ v))
                  ps2 = map g $ sampleVectors (opts^.envelopeNumPoints)
               in  map fst ps2 ++ map snd ps2
            EmptyEnvelope -> []

-- | Mark the envelope with an approximating cubic spline
--   using 32 points, medium line width and red line color.
showEnvelope :: Diagram V2 -> Diagram V2
showEnvelope = showEnvelope' def

------------------------------------------------------------------------
-- Approximating the trace
------------------------------------------------------------------------

-- | Options for displaying the envelope approximation.
data TraceOpts = TraceOpts
  { _traceStyle     :: Style V2 Double
  , _traceName      :: Name
  , _traceMarker    :: Diagram V2
  , _traceNumPoints :: Int
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''TraceOpts

traceStyle :: Lens' TraceOpts (Style V2 Double)
traceName  :: Lens' TraceOpts Name

-- | Number of traces calculated used to visualise the trace.
--
--   Default is @64@.
traceNumPoints :: Lens' TraceOpts Int

-- | The size of the
--
--   Default is a cross with length @'normalized' (1/40)@
traceMarker :: Lens' TraceOpts (Diagram V2)


type instance V TraceOpts = V2
type instance N TraceOpts = Double

instance Default TraceOpts where
  def = TraceOpts
    { _traceStyle     = mempty & lc red & lw medium
    , _traceName      = mempty
    , _traceMarker    = measuredDiagram (cross <$> normalized (1/80))
    , _traceNumPoints = 32
    }
    where
    cross x = (mkP2 (-x) (-x) ~~ mkP2 x x) <> (mkP2 (-x) x ~~ mkP2 x (-x))


instance ApplyStyle TraceOpts where
  applyStyle s = style <>~ s

instance HasStyle TraceOpts where
  style = traceStyle

instance Qualifiable TraceOpts
instance HasName TraceOpts where
  name = traceName

-- | Mark the trace of a diagram, with control over colour and scale
-- of marker dot and the number of points on the trace.
showTrace' :: TraceOpts -> Diagram V2 -> Diagram V2
showTrace' opts d = (opts^.name .>> foldMap moveTo ps m) <> d
  where
    -- another possibility is to trace along a regular grid formed from
    -- the bounding box
    vs  = sampleVectors (opts^.traceNumPoints)
    f v = fmap (\n -> P (n *^ v)) $ appTrace (getTrace d) origin v
    ps  = foldMap f vs
    m   = opts^.traceMarker & applyStyle (opts^.style)

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

