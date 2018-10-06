{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Render
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Types to specify lighting for 3D rendering.
--
-----------------------------------------------------------------------------

module Diagrams.ThreeD.Light where

import           Data.Colour
import           Data.Monoid
import           Data.Typeable

import           Geometry.Space
import           Geometry.Transform
import           Geometry.Direction
import           Geometry.ThreeD.Types
import           Geometry.Query

import Linear.Affine (Point, origin)

import Diagrams.Types

-- Point light ---------------------------------------------------------

-- | A @PointLight@ radiates uniformly in all directions from a given
-- point.
data PointLight = PointLight !(Point V3 Double) !(Colour Double)
  deriving Typeable

type instance V PointLight = V3
type instance N PointLight = Double

instance Transformable PointLight where
  transform t (PointLight p c) = PointLight (papply t p) c

-- | Construct a Diagram with a single PointLight at the origin, which
-- takes up no space.
pointLight :: Colour Double -> Diagram V3
pointLight c = mkQD (Prim $ PointLight origin c) mempty mempty
               (Query . const . Any $ False)

-- Parallel light ------------------------------------------------------

-- | A @ParallelLight@ casts parallel rays in the specified direction,
--   from some distant location outside the scene.
data ParallelLight = ParallelLight (V3 Double) (Colour Double)
  deriving Typeable

type instance V ParallelLight = V3
type instance N ParallelLight = Double

instance Transformable ParallelLight where
  transform t (ParallelLight v c) = ParallelLight (apply t v) c

-- | Construct a Diagram with a single ParallelLight, which takes up no space.
parallelLight
  :: Direction V3 Double -- ^ The direction in which the light travels.
  -> Colour Double  -- ^ The color of the light.
  -> Diagram V3
parallelLight d c = mkQD (Prim $ ParallelLight (fromDirection d) c)
                    mempty mempty (Query . const . Any $ False)

