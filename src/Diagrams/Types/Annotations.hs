{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}
-- We have some orphan Action instances here, but since Action is a multi-param
-- class there is really no better place to put them.

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Types
-- Copyright   :  (c) 2016 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Definitions of annotations used on the diagrams tree. There are
-- three separate types of annotations: up, down and static.
--
-----------------------------------------------------------------------------

module Diagrams.Types.Annotations
  (
    -- * Annotations

    -- ** Up annotations
    UpAnnots (..)
  , emptyUp
  , upEnvelope
  , upTrace
  , upQuery

   -- * Up modify
  , UpModify (..)

    -- * Down annotations
  , DownAnnots
  , downStyle

    -- *** Static annotations
  , AnnotKind (..)
  , Annotation
  , AnnotationClass (..)
  , AnnotationSpace
  , mkAnnot
  , getAnnot
  ) where

import           Control.Lens          hiding (transform)
import           Data.Monoid.Action
import           Data.Monoid.Coproduct.Strict
import           Data.Typeable
import           GHC.Exts

import           Geometry.Envelope
import           Geometry.Query
import           Geometry.Space
import           Geometry.Trace
import           Geometry.Transform    hiding (T)
import           Linear.Metric

import           Diagrams.Types.Style


------------------------------------------------------------------------
-- Annotations
------------------------------------------------------------------------

-- Up annotations ------------------------------------------------------

-- | Monoidal annotations which can be viewed from the top of the
--   diagram.
--
--   * envelopes (see "Geometry.Envelope"), which can be modified with
--     the 'UpMods'
--
--   * traces (see "Geometry.Trace"), also modifiable
--
--   * query functions (see "Geometry.Query")
data UpAnnots v n m = UpAnnots (Envelope v n) (Trace v n) (Query v n m)

type instance V (UpAnnots v n m) = v
type instance N (UpAnnots v n m) = n

instance (Metric v, HasLinearMap v, OrderedField n) => Transformable (UpAnnots v n m) where
  transform t (UpAnnots e tr q) =
    UpAnnots (transform t e) (transform t tr) (transform t q)
  {-# INLINE transform #-}

instance (Ord n, Semigroup m) => Semigroup (UpAnnots v n m) where
  UpAnnots e1 t1 q1 <> UpAnnots e2 t2 q2 = UpAnnots (e1<>e2) (t1<>t2) (q1<>q2)
  {-# INLINE (<>) #-}

instance Functor (UpAnnots v n) where
  fmap f = upQuery %~ fmap f
  {-# INLINE fmap #-}

-- | 'mempty' for 'UpAnnots' with less constraints.
emptyUp :: Monoid m => UpAnnots v n m
emptyUp =
  UpAnnots
    EmptyEnvelope
    mempty
    mempty
{-# INLINE emptyUp #-}

instance (Ord n, Semigroup m, Monoid m) => Monoid (UpAnnots v n m) where
  mempty = emptyUp
  {-# INLINE mempty  #-}
  mappend = (<>)
  {-# INLINE mappend #-}

-- note that lenses to envelope and trace for a diagram would not be
-- lawful, namely the functor law:
-- over l id == id
-- this is (setEnvelope . getEnvelope) and would force the envelope to
-- be whatever the current envelope it. If later we modified a
-- subdiagram, the change in envelope would not pass this point.

-- | Lens onto the envelope of an up annotation.
upEnvelope :: Lens' (UpAnnots v n m) (Envelope v n)
upEnvelope f (UpAnnots e t q) = f e <&> \e' -> UpAnnots e' t q
{-# INLINE upEnvelope #-}

-- | Lens onto the trace of an up annotation.
upTrace :: Lens' (UpAnnots v n m) (Trace v n)
upTrace f (UpAnnots e t q) = f t <&> \t' -> UpAnnots e t' q
{-# INLINE upTrace #-}

-- | Lens onto the envelope of an up annotation.
upQuery :: Lens (UpAnnots v n m) (UpAnnots v n m') (Query v n m) (Query v n m')
upQuery f (UpAnnots e t q) = f q <&> \q' -> UpAnnots e t q'
{-# INLINE upQuery #-}

-- Up modifier ---------------------------------------------------------

-- | Modifications to the envelope or trace of the diagram below.
data UpModify v n
  = EnvMod (Envelope v n -> Envelope v n)
  | TraceMod (Trace v n -> Trace v n)

instance (HasLinearMap v, OrderedField n) => Action (DownAnnots v n) (UpModify v n) where
  act d = \case
    EnvMod f   -> EnvMod   $ transform t . f . transform (inv t)
    TraceMod f -> TraceMod $ transform t . f . transform (inv t)
    where t = killR d

instance Action (UpModify v n) (UpAnnots v n m) where
  act (EnvMod f) = upEnvelope %~ f
  act (TraceMod f) = upTrace %~ f

-- Down annotations ----------------------------------------------------

-- | Monoidal annotations which travel down the diagram tree,
--   /i.e./ which accumulate along each path to a leaf (and which can
--   act on the up static annotations):
--
--   * styles (see "Diagrams.Types.Style")
--   * transform (see "Geometry.Transform")
type DownAnnots v n = Transformation v n :+: Style v n

downStyle
 :: (HasLinearMap v, OrderedField n)
 => Lens' (DownAnnots v n) (Style v n)
downStyle f d = f s <&> \s' -> inR s' <> inL t
  where (t, s) = untangle d

-- Static annotations --------------------------------------------------

-- | The two possible types of attributes:
--
--     * 'IAttr' - inert annotations that are unaffected by transforms
--     * 'TAttr' - transformable annotations
data AnnotKind = IAnnot | TAnnot

-- | Every annotation must be an instance of @AnnotationClass@.
class Typeable a => AnnotationClass a where
  -- | The type of annotation. Choose between 'IAnnot' and 'TAnnot'.
  type AnnotType a :: AnnotKind

class SingAnnot (i::AnnotKind) where
  sing :: AnnotS i

data AnnotS k where
  I :: k ~ 'IAnnot => AnnotS k
  T :: k ~ 'TAnnot => AnnotS k

instance SingAnnot 'IAnnot where sing = I
instance SingAnnot 'TAnnot where sing = T

type family Annotation' k a v n :: Constraint where
  Annotation' 'IAnnot a v n = AnnotType a ~ 'IAnnot
  Annotation' 'TAnnot a v n = (AnnotType a ~ 'TAnnot, InSpace v n a, Transformable a)

type AnnotationSpace a v n = (AnnotationClass a, Annotation' (AnnotType a) a v n, SingAnnot (AnnotType a))

-- | Static annotations are placed at the roots of diagrams and affect
--   everything below them. Unlike 'Style's, they are not pushed to each
--   'Prim', they stay where they are when rendering.
--
--   Annotations are used for things like 'opacityGroup's and 'clip's.
data Annotation v n where
  IAnnotation :: (AnnotationClass a, AnnotType a ~ 'IAnnot) => !a -> Annotation v n
  TAnnotation :: (AnnotationClass a, InSpace v n a, AnnotType a ~ 'TAnnot,
                  Transformable a) => !a -> Annotation v n

type instance V (Annotation v n) = v
type instance N (Annotation v n) = n

instance Transformable (Annotation v n) where
  transform t (TAnnotation a) = TAnnotation (transform t a)
  transform _ iannot          = iannot
  {-# INLINE transform #-}

-- | Annotations ignore styles.
instance ApplyStyle (Annotation v n) where
  applyStyle _ a = a

-- | Construct an annotation given a review onto the internal type.
mkAnnot :: AnnotationSpace a v n => AReview a r -> r -> Annotation v n
mkAnnot = mkAnnot' . review
{-# INLINE mkAnnot #-}

mkAnnot' :: forall a v n r. AnnotationSpace a v n => (r -> a) -> r -> Annotation v n
mkAnnot' ra r =
  case sing :: AnnotS (AnnotType a) of
    I -> IAnnotation (ra r)
    T -> TAnnotation (ra r)

-- | Extract an annotation. Used by backends.
getAnnot :: AnnotationClass a => Getting r a r -> Annotation v n -> Maybe r
getAnnot l = fromAnnot (view l)
{-# INLINE getAnnot #-}

fromAnnot :: forall a r v n. AnnotationClass a => (a -> r) -> Annotation v n -> Maybe r
fromAnnot ar = \case
  IAnnotation a -> case eq a of Just Refl -> Just (ar a); Nothing -> Nothing
  TAnnotation a -> case eq a of Just Refl -> Just (ar a); Nothing -> Nothing
  where
    eq :: Typeable a' => a' -> Maybe (a' :~: a)
    eq _ = eqT

