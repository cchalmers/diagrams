{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds          #-}
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
-- There are three separate types of annotations: up, down and static.
--
-----------------------------------------------------------------------------

module Diagrams.Types.Annotations
  (
    -- * Annotations

    -- ** Up annotations
    UpAnnots (..)
  , emptyUp
  , upEnvelope, upEnvelopeD
  , upTrace, upTraceD
  , upQuery

    -- * Down annotations
  , DownAnnots

    -- *** Static annotations
  , AnnotKind (..)
  , Annotation
  , AnnotationClass (..)
  , AnnotationSpace
  , mkAnnot
  , getAnnot
  ) where

import           Control.Lens              hiding (transform) -- (Lens', Prism', Rewrapped,
import           Data.Semigroup
import           Data.Typeable

import           Data.Monoid.Coproduct
import           Data.Monoid.Deletable
-- import           Data.Monoid.WithSemigroup

-- import           Geometry.Align
import           Geometry.Envelope
-- import           Geometry.HasOrigin
-- import           Geometry.Juxtapose
-- import           Geometry.Points
import           Geometry.Query
import           Geometry.Trace
import           Geometry.Transform hiding (T)
import           Geometry.Space

import           Diagrams.Types.Style
import GHC.Exts

-- import           Linear.Affine
import           Linear.Metric
-- import           Linear.Vector

------------------------------------------------------------------------
-- Annotations
------------------------------------------------------------------------

-- Up annotations ------------------------------------------------------

-- | Monoidal annotations which travel up the diagram tree, /i.e./ which
--   are aggregated from component diagrams to the whole:
--
--   * envelopes (see "Diagrams.Core.Envelope").
--     The envelopes are \"deletable\" meaning that at any point we can
--     throw away the existing envelope and replace it with a new one;
--     sometimes we want to consider a diagram as having a different
--     envelope unrelated to its \"natural\" envelope.
--
--   * traces (see "Diagrams.Core.Trace"), also
--     deletable.
--
--   * name/subdiagram associations (see "Diagrams.Core.Names")
--
--   * query functions (see "Diagrams.Core.Query")
data UpAnnots v n m = UpAnnots
  (Deletable (Envelope v n))
  (Deletable (Trace v n))
  (Query v n m)
-- Envelope and trace are 'Deleteable' so we can replace them. Just
-- replacing the top level up annotation isn't enough because we want to
-- be able to rebuild the up annotations when modifying a subdiagram.

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
    (toDeletable EmptyEnvelope)
    (toDeletable (Trace $ \_ _ -> unsafeMkSortedList []))
    mempty
{-# INLINE emptyUp #-}

instance (Ord n, Semigroup m, Monoid m) => Monoid (UpAnnots v n m) where
  mempty = emptyUp
  {-# INLINE mempty  #-}
  mappend = (<>)
  {-# INLINE mappend #-}

deletable :: Lens' (Deletable a) a
deletable f (Deletable a x b) = f x <&> \x' -> Deletable a x' b
{-# INLINE deletable #-}

-- note that lenses to envelope and trace for a diagram would not be
-- lawful, namely the functor law:
-- over l id == id
-- this is (setEnvelope . getEnvelope) would force the envelope to be
-- whatever the current envelope it. If later we modified a subdiagram,
-- the change in envelope would not pass this point.

-- | Lens onto the envelope of an up annotation.
upEnvelope :: Lens' (UpAnnots v n m) (Envelope v n)
upEnvelope = upEnvelopeD . deletable
{-# INLINE upEnvelope #-}

upEnvelopeD :: Lens' (UpAnnots v n m) (Deletable (Envelope v n))
upEnvelopeD f (UpAnnots e t q) = f e <&> \e' -> UpAnnots e' t q
{-# INLINE upEnvelopeD #-}

-- | Lens onto the envelope of an up annotation.
upTrace :: Lens' (UpAnnots v n m) (Trace v n)
upTrace = upTraceD . deletable
{-# INLINE upTrace #-}

upTraceD :: Lens' (UpAnnots v n m) (Deletable (Trace v n))
upTraceD f (UpAnnots e t q) = f t <&> \t' -> UpAnnots e t' q
{-# INLINE upTraceD #-}

-- | Lens onto the envelope of an up annotation.
upQuery :: Lens (UpAnnots v n m) (UpAnnots v n m') (Query v n m) (Query v n m')
upQuery f (UpAnnots e t q) = f q <&> \q' -> UpAnnots e t q'
{-# INLINE upQuery #-}

-- Down annotations ----------------------------------------------------

-- | Monoidal annotations which travel down the diagram tree,
--   /i.e./ which accumulate along each path to a leaf (and which can
--   act on the up static annotations):
--
--   * styles (see "Diagrams.Types.Style")
--   * transform (see "Geometry.Transform")
type DownAnnots v n = Transformation v n :+: Style v n

-- Static annotations --------------------------------------------------

-- | The two possible types of attributes:
--
--     * 'IAttr' - inert annotations that are unaffected by transforms
--     * 'TAttr' - transformable annotations
data AnnotKind = IAnnot | TAnnot

-- | Every attribute must be an instance of @AttributeClass@  The
--   'Semigroup' instance for an attribute determines how it will combine
--   with other attributes of the same type.
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

-- Specific annotations ------------------------------------------------

-- newtype Href = Href String

-- _Href :: Iso' Href String
-- _Href = coerced

-- newtype OpacityGroup = OpacityGroup String

-- _OpacityGroup :: Iso' OpacityGroup Double
-- _OpacityGroup = coerced

-- opacityGroup :: Double -> QDiagram v n m -> QDiagram v n m
-- opacityGroup = applyAnnot _OpacityGroup

-- renderAnnot annot
--   | Just o <- getAnnot _OpacityGroup annot

--   = Href String    -- ^ Hyperlink
--   | OpacityGroup Double

-- -- | Static annotations which can be placed at a particular node of a
-- --   diagram tree.
-- data Annotation v n
--   = Href String    -- ^ Hyperlink
--   | OpacityGroup Double
--   | Shading (QDiagram v n Any)
--   | AClip !(Transformation v n) (Seq (UPath v n))
--      -- The transformation gets applied the same time as rendering.

-- | The shading diagram gets tranformed.
-- instance (Metric v, HasLinearMap v, OrderedField n)
--     => Transformable (Annotation v n) where
--   transform t = \case
--     Shading dia -> Shading (transform t dia)
--     AClip t0 ps -> AClip (t <> t0) ps
--     a           -> a

-- I don't think we want any styles to be applied to the shading
-- diagram.
-- instance ApplyStyle (Annotation v n) where
--   applyStyle _ = id

