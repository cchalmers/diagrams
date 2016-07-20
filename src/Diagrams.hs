{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams
-- Copyright   :  (c) 2015 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module only contains exports defined in @diagrams-lib@ or
-- @diagrams-core@. This module can be used if you want to avoid some
-- potential conflicts with other modules, but importing
-- "Diagrams.Prelude" (which includes re-exports from other packages)
-- is often more convenient.
--
-----------------------------------------------------------------------------

module Diagrams
  (
    -- * Core library
    -- | The core definitions of transformations, diagrams,
    --   backends, and so on.
    -- module Diagrams.Core

    -- * Standard library

    -- | Attributes (color, line style, etc.) and styles.
    module Diagrams.Attributes

    -- | Alignment of diagrams relative to their envelopes.
  , module Geometry.Align

    -- | Creating and using bounding boxes.
  , module Geometry.BoundingBox

    -- | Combining multiple diagrams into one.
  , module Geometry.Combinators

    -- | Giving concrete locations to translation-invariant things.
  , module Geometry.Located

    -- | Linear and cubic bezier segments.
  , module Geometry.Segment

    -- | Trails.
  , module Geometry.Trail

    -- | Parametrization of segments and trails.
  , module Geometry.Parametric

    -- | Adjusting the length of parameterized objects.
  -- , module Diagrams.Parametric.Adjust

    -- | Computing tangent and normal vectors of segments and
    --   trails.
  , module Geometry.Tangent

    -- | Trail-like things.
  , module Geometry.TrailLike

    -- | Paths.
  , module Geometry.Path

    -- | Cubic splines.
  -- , module Diagrams.CubicSpline

    -- | Some additional transformation-related functions, like
    --   conjugation of transformations.
  , module Geometry.Transform

    -- | Projective transformations and other deformations
    -- lacking an inverse.
  -- , module Diagrams.Deform

    -- | Giving names to subdiagrams and later retrieving
    --   subdiagrams by name.
  -- , module Diagrams.Names

    -- | Envelopes, aka functional bounding regions.
  , module Geometry.Envelope

    -- | Traces, aka embedded raytracers, for finding points on
    --   the boundary of a diagram.
  , module Geometry.Trace

    -- | A query is a function that maps points in a vector space
    --   to values in some monoid; they can be used to annotate
    --   the points of a diagram with some values.
  , module Geometry.Query

    -- | Utilities for working with points.
  , module Geometry.Points

    -- | Utilities for working with size.
  , module Geometry.Size

    -- | Angles
  , module Geometry.Angle

    -- | Convenience infix operators for working with coordinates.
  -- , module Diagrams.Coordinates

    -- | Directions, distinguished from angles or vectors
  , module Geometry.Direction

    -- | A wide range of things (shapes, transformations,
    --   combinators) specific to creating two-dimensional
    --   diagrams.
  , module Diagrams.TwoD.Combinators

  , module Diagrams.TwoD.Model

  , module Diagrams.TwoD.Path.Unboxed
  , module Diagrams.TwoD.Attributes

    -- | Extra things for three-dimensional diagrams.
  -- , module Diagrams.ThreeD

    -- | Tools for making animations.
  -- , module Diagrams.Animation

    -- | Various utility definitions.
  , module Diagrams.Util

  ) where

import           Geometry.Align
import           Geometry.Angle
-- import           Diagrams.Animation
import           Diagrams.Attributes
import           Diagrams.TwoD.Attributes
import           Diagrams.TwoD.Path.Unboxed
import           Diagrams.TwoD.Model
import           Diagrams.TwoD.Combinators
import           Geometry.BoundingBox
import           Geometry.Combinators
-- import           Diagrams.Coordinates
-- import           Diagrams.CubicSpline
-- import           Diagrams.Deform
import           Geometry.Direction         hiding (dir)
import           Geometry.Envelope
import           Geometry.Located
import           Geometry.Parametric
-- import           Diagrams.Parametric.Adjust
import           Geometry.Path              hiding (pathPoints)
import           Geometry.Points
import           Geometry.Query
import           Geometry.Segment
import           Geometry.Size
import           Geometry.Tangent
-- import           Diagrams.ThreeD
import           Geometry.Trace
import           Geometry.Trail             hiding (linePoints, loopPoints,
                                             trailPoints)
import           Geometry.TrailLike
import           Geometry.Transform
-- import           Diagrams.TwoD
import           Diagrams.Util

