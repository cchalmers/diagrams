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
    -- * Core types
    QDiagram
  , Diagram
  , Name

    -- ** Measures
  , Measured
  , Measure
  , output
  , local
  , global
  , normalized
  , normalised
  , atLeast
  , atMost

    -- * Standard library

    -- | Attributes (color, line style, etc.) and styles.
  , module Diagrams.Attributes

  , module Diagrams.TwoD.Attributes
  , module Diagrams.TwoD.Path

    -- | Alignment of diagrams relative to their envelopes.
  -- , module Geometry.Align

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
  -- , module Geometry.Tangent

    -- | Trail-like things.
  -- , module Geometry.TrailLike

    -- | Paths.
  -- , module Geometry.Path

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

  -- , module Diagrams.TwoD.Path.Unboxed

    -- | Extra things for three-dimensional diagrams.
  -- , module Diagrams.ThreeD

    -- | Tools for making animations.
  , module Diagrams.Animation

    -- | Various utility definitions.
  , module Diagrams.Util

  , module Geometry.TwoD.Types
  , module Geometry.ThreeD.Types
  , module Geometry.TwoD.Vector
  , module Geometry.Space

  , module Diagrams.Types.Style
  , module Diagrams.Types.Measure
  , module Diagrams.Types.Annotations
  , module Diagrams.Types.Names

  , module Geometry.Path
  , module Geometry.Trail
  , module Geometry.Segment

  , module Geometry.TwoD.Shapes
  , module Diagrams.TwoD.Combinators
  , module Diagrams.Combinators

  ) where


import           Diagrams.Animation
import           Diagrams.Attributes
import           Diagrams.Combinators
import           Diagrams.TwoD.Attributes
import           Diagrams.TwoD.Combinators
import           Diagrams.TwoD.Model
import           Diagrams.TwoD.Path
import           Diagrams.Types
import           Diagrams.Types.Annotations
import           Diagrams.Types.Measure
import           Diagrams.Types.Names
import           Diagrams.Types.Style
import           Diagrams.Util
import           Geometry.Angle
import           Geometry.BoundingBox
import           Geometry.Combinators
import           Geometry.Direction         hiding (dir)
import           Geometry.Envelope
import           Geometry.Located
import           Geometry.Parametric
import           Geometry.Path
import           Geometry.Points
import           Geometry.Query
import           Geometry.Segment
import           Geometry.Size
import           Geometry.Space
import           Geometry.ThreeD.Types
import           Geometry.Trace
import           Geometry.Trail
import           Geometry.Transform
import           Geometry.TwoD.Shapes
import           Geometry.TwoD.Types
import           Geometry.TwoD.Vector

