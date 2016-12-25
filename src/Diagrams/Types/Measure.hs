{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Diagrams.Types.Measure
  ( Measured (..)
  , Measure
  , fromMeasured
  , output
  , local
  , global
  , normalized
  , normalised
  , scaleLocal
  , atLeast
  , atMost
  ) where

import           Control.Applicative
import           Control.Lens
import qualified Control.Monad.Reader as R
import           Data.Distributive
import           Data.Functor.Rep
import           Data.Semigroup
import           Data.Typeable

import           Geometry.Space

import           Linear.Vector



-- | 'Measured n a' is an object that depends on 'local', 'normalized'
--   and 'global' scales. The 'normalized' and 'global' scales are
--   calculated when rendering a diagram.
--
--   For attributes, the 'local' scale gets multiplied by the average
--   scale of the transform.
newtype Measured a = Measured { unmeasure :: (Double,Double,Double) -> a }
  deriving (Typeable, Functor, Applicative, Monad, Additive, R.MonadReader (Double,Double,Double))
-- (local, global, normalized) -> output

type instance V (Measured a) = V a
type instance N (Measured a) = Double

-- | A measure is a 'Measured' number.
type Measure = Measured Double

-- | @fromMeasured globalScale normalizedScale measure -> a@
fromMeasured :: Double -> Double -> Measured a -> a
fromMeasured g n (Measured m) = m (1,g,n)

-- | Output units don't change.
output :: Double -> Measure
output = pure

-- | Local units are scaled by the average scale of a transform.
local :: Double -> Measure
local x = views _1 (*x)

-- | Global units are ?
global :: Double -> Measure
global x = views _2 (*x)

-- | Normalized units get scaled so that one normalized unit is the size of the
--   final diagram.
normalized :: Double -> Measure
normalized x = views _3 (*x)

-- | Just like 'normalized' but spelt properly.
normalised :: Double -> Measure
normalised x = views _3 (*x)

-- | Scale the local units of a 'Measured' thing.
scaleLocal :: Double -> Measured a -> Measured a
scaleLocal s = R.local (_1 *~ s)

-- | Calculate the smaller of two measures.
atLeast :: Measure -> Measure -> Measure
atLeast = liftA2 max

-- | Calculate the larger of two measures.
atMost :: Measure -> Measure -> Measure
atMost = liftA2 min

instance Num a => Num (Measured a) where
  (+) = (^+^)
  (-) = (^-^)
  (*) = liftA2 (*)

  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

instance Fractional a => Fractional (Measured a) where
  (/)   = liftA2 (/)
  recip = fmap recip

  fromRational = pure . fromRational

instance Floating a => Floating (Measured a) where
  pi      = pure pi
  exp     = fmap exp
  sqrt    = fmap sqrt
  log     = fmap log
  (**)    = liftA2 (**)
  logBase = liftA2 logBase
  sin     = fmap sin
  tan     = fmap tan
  cos     = fmap cos
  asin    = fmap asin
  atan    = fmap atan
  acos    = fmap acos
  sinh    = fmap sinh
  tanh    = fmap tanh
  cosh    = fmap cosh
  asinh   = fmap asinh
  atanh   = fmap atanh
  acosh   = fmap acosh

instance Semigroup a => Semigroup (Measured a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Measured a) where
  mempty  = pure mempty
  mappend = liftA2 mappend

instance Distributive Measured where
  distribute a = Measured $ \x -> fmap (\(Measured m) -> m x) a

instance Representable Measured where
  type Rep Measured = (Double,Double,Double)
  tabulate = Measured
  index    = unmeasure

