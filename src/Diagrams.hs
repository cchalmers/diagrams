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
-- Notable emmitions from the this module to avoid potential comflicts:
--   - "Diagrams.TwoD.Image"
--   - "Diagrams.TwoD.Text"
--
-- Backends will also want to import "Diagrams.Backend.Compile".
--
-----------------------------------------------------------------------------

module Diagrams
  (
    -- * Diagram
    QDiagram
  , Diagram

  , pointDiagram

  , named
  , localize
  , styles
  , leafs
  , releaf
  , down

    -- * Subdiagrams
  , SubDiagram
  , getSub
  , modSub
  , subLocation
  , allSubs
  , findSubs

    -- * Standard library

    -- | The geometry library used by diagrams. Includes
    --   - 'Angle'
    --   - 'BoundingBox'
    --   - 'Envelope'
    --   - 'Direction'
    --   - 'Located'
    --   - 'Path'
    --   - 'Query'
    --   - 'Segment'
    --   - 'Trace'
    --   - 'Trail'
    --   - 'Transformation'
  , module Geometry

    -- | Tools for making animations.
  , module Diagrams.Animation

    -- | General attributes ('LineWidth', 'Opacity' etc.).
  , module Diagrams.Attributes

    -- | The 'Backend' class, used to render diagrams by one of the
    -- various backends.
  , module Diagrams.Backend

    -- | The command line interface for generating diagrams.
  , module Diagrams.Backend.CmdLine

    -- | General combinators ('withEnvelope', 'pad', 'frame' etc.). See also
    -- "Geometry.Combinators" (exported by "Geometry").
  , module Diagrams.Combinators

    -- | TwoD Attributes ('FillTexture', 'LineTexture', 'Clip' , etc.).
  , module Diagrams.TwoD.Attributes

    -- | General combinators ('strutX', 'padY', 'bg' etc.). See also
    -- "Geometry.TwoD.Combinators" (exported by "Geometry").
  , module Diagrams.TwoD.Combinators

    -- | Stroking paths. See also "Geometry.Path" (exported by
    -- "Geometry").
  , module Diagrams.TwoD.Path

  -- | Utilities for illustrating or debugging diagrams ('showOrigin',
  -- 'showEnvelope' etc.)
  , module Diagrams.TwoD.Model

    -- | Projective transformations and other deformations
    -- lacking an inverse.
  -- , module Diagrams.Deform

    -- | Giving names to subdiagrams and later retrieving
    --   subdiagrams by name.
  -- , module Diagrams.Names

    -- | Extra things for three-dimensional diagrams.
  -- , module Diagrams.ThreeD

    -- | Various utility definitions ('#', 'with', 'tau', etc.).
  , module Diagrams.Util

    -- | Collections of attibutes.
  , module Diagrams.Types.Style

  , module Diagrams.Types.Measure
  , module Diagrams.Measured
  , module Diagrams.Types.Annotations
  , module Diagrams.Types.Names

  ) where

import           Diagrams.Animation
import           Diagrams.Attributes
import           Diagrams.Backend
import           Diagrams.Backend.CmdLine
import           Diagrams.Combinators
import           Diagrams.Measured
import           Diagrams.TwoD.Attributes
import           Diagrams.TwoD.Combinators
import           Diagrams.TwoD.Model
import           Diagrams.TwoD.Path
import           Diagrams.Types
import           Diagrams.Types.Annotations
import           Diagrams.Types.Measure     hiding (Measured (..))
import           Diagrams.Types.Measure     (Measure)
import           Diagrams.Types.Names
import           Diagrams.Types.Style       hiding (Style (..), _Style)
import           Diagrams.Types.Style       (Style)
import           Diagrams.Util

import           Geometry

