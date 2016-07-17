{-# LANGUAGE CPP                   #-}
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
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}
-- We have some orphan Action instances here, but since Action is a multi-param
-- class there is really no better place to put them.

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Types
-- Copyright   :  (c) 2011-2016 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The core library of primitives forming the basis of an embedded
-- domain-specific language for describing and rendering diagrams.
--
-- "Diagrams.Core.Types" defines types and classes for
-- primitives, diagrams, and backends.
--
-----------------------------------------------------------------------------

{- ~~~~ Note [breaking up Types module]

   Although it's not as bad as it used to be, this module has a lot of
   stuff in it, and it might seem a good idea in principle to break it up
   into smaller modules.  However, it's not as easy as it sounds: everything
   in this module cyclically depends on everything else.
-}

module Diagrams.Types
  (
    -- * Diagrams

    -- ** Annotations

    -- *** Static annotations
    Annotation(..)
  , applyAnnotation
  , href
  , opacityGroup, groupOpacity
  , shading

    -- *** Dynamic (monoidal) annotations
  , UpAnnots, DownAnnots
  , upEnvelope
  , upTrace
  , upQuery

    -- ** Basic type definitions
  , QDiaLeaf(..) -- , withQDiaLeaf
  , QDiagram(..), Diagram

    -- * Operations on diagrams
    -- ** Creating diagrams
  , primQD, mkQD, mkQD', pointDiagram

    -- ** Combining diagrams

    -- | For many more ways of combining diagrams, see
    --   "Diagrams.Combinators" and "Diagrams.TwoD.Combinators"
    --   from the diagrams-lib package.

  -- , atop

    -- ** Modifying diagrams
    -- *** Names
  -- , nameSub
  -- , lookupName
  -- , withName
  -- , withNameAll
  -- , withNames
  -- , localize

    -- *** Replaceing up annotations
  , setEnvelope
  , setTrace
  , upDiagram
  , upWith

    -- * Subdiagrams

  , Subdiagram(..) -- , mkSubdiagram
  -- , getSub, rawSub
  -- , location
  -- , subPoint

    -- * Subdiagram maps

  , SubMap(..)

  -- , fromNames, rememberAs, lookupSub

    -- * Primtives
    -- $prim

  , Prim(..)
  , _Prim

    -- ** Number classes
  , TypeableFloat

  ) where

-- import           Control.Arrow             (first, second, (***))
import           Control.Lens              hiding (transform) -- (Lens', Prism', Rewrapped,
                                            -- Wrapped (..), iso, lens, over,
                                            -- prism', view, (^.), _Wrapped,
                                            -- _Wrapping, (<&>), (&), (.~))
-- import           Control.Monad             (mplus)
-- import           Data.List                 (isSuffixOf)
-- import qualified Data.Map                  as M
-- import           Data.Maybe                (fromMaybe, listToMaybe)
import           Data.Semigroup
-- import qualified Data.Traversable          as T
-- import           Data.Tree
import           Data.Typeable

-- import           Data.Monoid.Action
import           Data.Monoid.Coproduct
import           Data.Monoid.Deletable
-- import           Data.Monoid.MList
import           Data.Monoid.WithSemigroup
import qualified Data.Tree.DUAL.Label      as D
import Data.Sequence (Seq)
import Geometry.Path.Unboxed (UPath)

import           Geometry.Align
import           Geometry.Envelope
import           Geometry.HasOrigin
import           Geometry.Juxtapose
import           Geometry.Points
import           Geometry.Query
import           Geometry.Trace
import           Geometry.Transform
import           Geometry.Space

import           Diagrams.Names
import           Diagrams.Style

-- import           Linear.Affine
import           Linear.Metric
-- import           Linear.Vector

-- XXX TODO: add lots of actual diagrams to illustrate the
-- documentation!  Haddock supports \<\<inline image urls\>\>.

-- | Class of numbers that are 'RealFloat' and 'Typeable'. This class is used to
--   shorten type constraints.
class (Typeable n, RealFloat n) => TypeableFloat n
instance (Typeable n, RealFloat n) => TypeableFloat n
-- use class instead of type constraint so users don't need constraint kinds pragma

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
-- type UpAnnots b v n m = Deletable (Envelope v n)
--                     ::: Deletable (Trace v n)
--                     ::: Query v n m
--                     ::: ()

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
--   act on the upwards-travelling and static annotations):
--
--   * styles (see "Diagrams.Core.Style")
type DownAnnots v n = Transformation v n :+: Style v n

-- Static annotations --------------------------------------------------

-- | Static annotations which can be placed at a particular node of a
--   diagram tree.
data Annotation v n
  = Href String    -- ^ Hyperlink
  | OpacityGroup Double
  | Shading (QDiagram v n Any)
  | AClip !(Transformation v n) (Seq (UPath v n))
     -- The transformation gets applied the same time as rendering.

type instance V (Annotation v n) = v
type instance N (Annotation v n) = n

-- | The shading diagram gets tranformed.
instance (Metric v, HasLinearMap v, OrderedField n)
    => Transformable (Annotation v n) where
  transform t = \case
    Shading dia -> Shading (transform t dia)
    AClip t0 ps -> AClip (t <> t0) ps
    a           -> a

-- I don't think we want any styles to be applied to the shading
-- diagram.
instance ApplyStyle (Annotation v n) where
  applyStyle _ = id

-- | Apply a static annotation at the root of a diagram.
applyAnnotation :: Annotation v n -> QDiagram v n m -> QDiagram v n m
applyAnnotation an (QD dt) = QD (D.annot an dt)

-- | Make a diagram into a hyperlink. Note that only some backends
--   will honor hyperlink annotations.
href :: String -> QDiagram v n m -> QDiagram v n m
href = applyAnnotation . Href

-- | Change the transparency of a 'Diagram' as a group.
opacityGroup, groupOpacity :: Double -> QDiagram v n m -> QDiagram v n m
opacityGroup = applyAnnotation . OpacityGroup
groupOpacity = applyAnnotation . OpacityGroup

-- | Apply a shading to a diagram. That is, use the lumosity
--   (brightness) of the first diagram as the alpha channel for the
--   second diagram.
--
--   -- XXX List official backends that support this.
shading :: QDiagram v n any -> QDiagram v n m -> QDiagram v n m
shading = applyAnnotation . Shading . (mempty <$)

------------------------------------------------------------------------
-- Primitives
------------------------------------------------------------------------

-- $prim
-- Ultimately, every diagram is essentially a tree whose leaves are
-- /primitives/, basic building blocks which can be rendered by
-- backends.  However, not every backend must be able to render every
-- type of primitive.

-- | A value of type @Prim b v n@ is an opaque (existentially quantified)
--   primitive which backend @b@ knows how to render in vector space @v@.
data Prim v n where
  Prim :: Typeable p => p -> Prim (V p) (N p)

type instance V (Prim v n) = v
type instance N (Prim v n) = n

-- | Prism onto a 'Prim'.
_Prim :: (InSpace v n p, Typeable p) => Prism' (Prim v n) p
_Prim = prism' Prim (\(Prim p) -> cast p)
{-# INLINE _Prim #-}

-- | A leaf in a 'QDiagram' tree is either a 'Prim', or a \"delayed\"
--   @QDiagram@ which expands to a real @QDiagram@ once it learns the
--   \"final context\" in which it will be rendered.  For example, in
--   order to decide how to draw an arrow, we must know the precise
--   transformation applied to it (since the arrow head and tail are
--   scale-invariant).
data QDiaLeaf v n m
  = PrimLeaf (Prim v n)
  | DelayedLeaf (DownAnnots v n -> n -> n -> QDiagram v n m)
  -- ^ The @QDiagram@ produced by a @DelayedLeaf@ function /must/
  --   already apply any transformation in the given @DownAnnots@ (that
  --   is, the transformation will not be applied by the context).
  deriving Functor

-- | The fundamental diagram type.  The type variables are as follows:
--
--   * @v@ represents the vector space of the diagram.  Typical
--     instantiations include @V2@ (for a two-dimensional diagram) or
--     @V3@ (for a three-dimensional diagram).
--
--   * @n@ represents the numerical field the diagram uses.  Typically
--     this will be a concrete numeric type like @Double@.
--
--   * @m@ is the monoidal type of \"query annotations\": each point
--     in the diagram has a value of type @m@ associated to it, and
--     these values are combined according to the 'Monoid' instance
--     for @m@.  Most often, @m@ is simply instantiated to 'Any',
--     associating a simple @Bool@ value to each point indicating
--     whether the point is inside the diagram; 'Diagram' is a synonym
--     for @QDiagram@ with @m@ thus instantiated to @Any@.
--
--   Diagrams can be combined via their 'Monoid' instance, transformed
--   via their 'Transformable' instance, and assigned attributes via
--   their 'ApplyStyle' instance.
--
--   Note that the @Q@ in @QDiagram@ stands for \"Queriable\", as
--   distinguished from 'Diagram', where @m@ is fixed to @Any@. This
--   is not really a very good name, but it's probably not worth
--   changing it at this point.
newtype QDiagram v n m =
  QD (
  D.IDUAL AName
          (DownAnnots v n)
          (UpAnnots v n m)
          (Annotation v n)
          (QDiaLeaf v n m)
  )

instance Wrapped (QDiagram v n m) where
  type Unwrapped (QDiagram v n m) = D.IDUAL AName (DownAnnots v n) (UpAnnots v n m) (Annotation v n) (QDiaLeaf v n m)
  _Wrapped' = iso (\(QD d) -> d) QD
  {-# INLINE _Wrapped' #-}

instance Rewrapped (QDiagram v n m) (QDiagram v' n' m')

getU :: QDiagram v n m -> Maybe (UpAnnots v n m)
getU (QD t) = D.getU t
{-# INLINE getU #-}

type instance V (QDiagram v n m) = v
type instance N (QDiagram v n m) = n

-- | @Diagram v@ is a synonym for @'QDiagram' v Double Any@. That is,
--   the default sort of diagram is one where querying at a point
--   simply tells you whether the diagram contains that point or not.
--   Transforming a default diagram into one with a more interesting
--   query can be done via the 'Functor' instance of @'QDiagram' v n@ or
--   the 'value' function.
type Diagram v = QDiagram v Double Any

-- | Construct a diagram made up from an up annotation.
upDiagram :: UpAnnots v n m -> QDiagram v n m
upDiagram = QD . D.leafU
{-# INLINE upDiagram #-}

-- | Construct a 'upDiagram' by apply a function to the empty up
--   annotations.
upWith :: Monoid m => (UpAnnots v n m -> UpAnnots v n m) -> QDiagram v n m
upWith f = upDiagram (f emptyUp)
{-# INLINE upWith #-}

-- | Create a \"point diagram\", which has no content, no trace, an
--   empty query, and a point envelope.
pointDiagram
  :: (Metric v, Fractional n, Monoid m)
  => Point v n
  -> QDiagram v n m
pointDiagram p = upWith $ upEnvelope .~ pointEnvelope p
{-# INLINE pointDiagram #-}

-- | Replace the envelope of a diagram.
setEnvelope
  :: (OrderedField n, Monoid' m)
  => Envelope v n -> QDiagram v n m -> QDiagram v n m
setEnvelope e = over _Wrapped'
  $ D.preapplyU  (emptyUp & upEnvelopeD .~ (toDeletable e <> deleteL))
  . D.postapplyU (emptyUp & upEnvelopeD .~ deleteR)
{-# INLINE setEnvelope #-}

-- | Replace the trace of a diagram.
setTrace :: (OrderedField n, Monoid' m)
         => Trace v n -> QDiagram v n m -> QDiagram v n m
setTrace t = over _Wrapped'
  $ D.preapplyU  (emptyUp & upTraceD .~ toDeletable t <> deleteL)
  . D.postapplyU (emptyUp & upTraceD .~ deleteR)
{-# INLINE setTrace #-}

-- | Get a list of names of subdiagrams and their locations.
-- names :: (Metric v, HasLinearMap v, Typeable n, Semigroup m, OrderedField n)
--       => QDiagram v n m -> [(Name, [Point v n])]
-- names = (map . second . map) location . M.assocs . view (subMap . _Wrapped')

-- | Lookup the most recent diagram associated with (some
--   qualification of) the given name.
-- lookupName :: (IsName nm, Metric v, HasLinearMap v, Typeable n, Semigroup m, OrderedField n)
--            => nm -> QDiagram v n m -> Maybe (Subdiagram b v n m)
-- lookupName n d = lookupSub (toName n) (d^.subMap) >>= listToMaybe

-- -- | \"Localize\" a diagram by hiding all the names, so they are no
-- --   longer visible to the outside.
-- localize :: forall b v n m. (Metric v, HasLinearMap v, Typeable n, OrderedField n, Semigroup m)
--          => QDiagram v n m -> QDiagram v n m
-- localize = over _Wrapped' ( D.applyUpre  (inj (deleteL :: Deletable (SubMap b v n m)))
--                    . D.applyUpost (inj (deleteR :: Deletable (SubMap b v n m)))
--                    )

-- | Make a QDiagram using the default envelope, trace and query for the
--   primitive.
primQD
  :: (InSpace v n a, Typeable a, Enveloped a, Traced a, HasQuery a m)
  => a
  -> QDiagram v n m
primQD a = mkQD (Prim a) (getEnvelope a) (getTrace a) (getQuery a)
{-# INLINE primQD #-}

-- | Create a diagram from a single primitive, along with an envelope,
--   trace, subdiagram map, and query function.
mkQD :: Prim v n -> Envelope v n -> Trace v n -> Query v n m -> QDiagram v n m
mkQD p = mkQD' (PrimLeaf p)
{-# INLINE mkQD #-}

-- | Create a diagram from a generic QDiaLeaf, along with an envelope,
--   trace, subdiagram map, and query function.
mkQD'
  :: QDiaLeaf v n m
  -> Envelope v n
  -> Trace v n
  -> Query v n m
  -> QDiagram v n m
mkQD' l e t q = QD $ D.leaf (UpAnnots (toDeletable e) (toDeletable t) q) l
{-# INLINE mkQD' #-}

------------------------------------------------------------
-- Instances
------------------------------------------------------------

-- | Diagrams form a monoid since each of their components do: the
--   empty diagram has no primitives, an empty envelope, an empty
--   trace, no named subdiagrams, and a constantly empty query
--   function.
--
--   Diagrams compose by aligning their respective local origins.  The
--   new diagram has all the primitives and all the names from the two
--   diagrams combined, and query functions are combined pointwise.
--   The first diagram goes on top of the second.  \"On top of\"
--   probably only makes sense in vector spaces of dimension lower
--   than 3, but in theory it could make sense for, say, 3-dimensional
--   diagrams when viewed by 4-dimensional beings.
instance (HasLinearMap v, OrderedField n, Semigroup m)
  => Monoid (QDiagram v n m) where
  mempty  = QD mempty
  {-# INLINE mempty  #-}
  mappend = (<>)
  {-# INLINE mappend #-}

instance (HasLinearMap v, OrderedField n, Semigroup m)
  => Semigroup (QDiagram v n m) where
  QD d1 <> QD d2 = QD (d2 <> d1)
  {-# INLINE (<>) #-}
    -- swap order so that primitives of d2 come first, i.e. will be
    -- rendered first, i.e. will be on the bottom.

instance Functor (QDiagram v n) where
  fmap f (QD d) = QD $ D.mapUAL (fmap f) id (fmap f) d
  {-# INLINE fmap #-}

instance (Metric v, HasLinearMap v, OrderedField n) => ApplyStyle (QDiagram v n m) where
  applyStyle = over _Wrapped' . D.down . inR
  {-# INLINE applyStyle #-}

instance Monoid m => HasQuery (QDiagram v n m) m where
  getQuery = maybe mempty (view upQuery) . getU
  {-# INLINE getQuery #-}

instance (Metric v, HasLinearMap v, OrderedField n)
    => Juxtaposable (QDiagram v n m) where
  juxtapose = juxtaposeDefault
  {-# INLINE juxtapose #-}

instance (Metric v, HasLinearMap v, OrderedField n)
    => Enveloped (QDiagram v n m) where
  getEnvelope = maybe EmptyEnvelope (view upEnvelope) . getU
  {-# INLINE getEnvelope #-}

instance (Metric v, HasLinearMap v, OrderedField n)
    => Traced (QDiagram v n m) where
  getTrace = maybe mempty (view upTrace) . getU
  {-# INLINE getTrace #-}

instance (Metric v, HasLinearMap v, OrderedField n)
    => HasOrigin (QDiagram v n m) where
  moveOriginTo = translate . (origin .-.)
  {-# INLINE moveOriginTo #-}

instance (OrderedField n, Metric v, HasLinearMap v)
    => Transformable (QDiagram v n m) where
  transform = over _Wrapped' . D.down . inL
  {-# INLINE transform #-}

instance (OrderedField n, Metric v, HasLinearMap v)
    => Alignable (QDiagram v n m) where
  defaultBoundary = envelopeBoundary
  {-# INLINE defaultBoundary #-}

-- | Diagrams can be qualified so that all their named points can
--   now be referred to using the qualification prefix.
-- instance (Metric v, HasLinearMap v, Typeable n, OrderedField n, Semigroup m)
--       => Qualifiable (QDiagram v n m) where
--   (.>>) = over _Wrapped' . D.applyD . inj . toName

------------------------------------------------------------
--  Subdiagrams
------------------------------------------------------------

-- | A @Subdiagram@ represents a diagram embedded within the context
--   of a larger diagram.  Essentially, it consists of a diagram
--   paired with any accumulated information from the larger context
--   (transformations, attributes, etc.).
newtype Subdiagram v n m = Subdiagram
  (D.SubIDUAL AName (DownAnnots v n) (UpAnnots v n m) (Annotation v n) (QDiaLeaf v n m))

type instance V (Subdiagram v n m) = v
type instance N (Subdiagram v n m) = n

-- | Turn a diagram into a subdiagram with no accumulated context.
-- mkSubdiagram :: QDiagram v n m -> Subdiagram v n m
-- mkSubdiagram d = Subdiagram d id id empty

-- | Get the location of a subdiagram; that is, the location of its
--   local origin /with respect to/ the vector space of its parent
--   diagram.  In other words, the point where its local origin
--   \"ended up\".
-- subLocation :: (Additive v, HasLinearMap v, Typeable n, Num n) => Subdiagram v n m -> Point v n
-- subLocation (Subdiagram _ _ d) = papply (transfFromAnnot a) origin
-- {-# INLINE subLocation #-}

-- | Turn a subdiagram into a normal diagram, including the enclosing
--   context.  Concretely, a subdiagram is a pair of (1) a diagram and
--   (2) a \"context\" consisting of an extra transformation and
--   attributes.  @getSub@ simply applies the transformation and
--   attributes to the diagram to get the corresponding \"top-level\"
--   diagram.
-- getSub :: (Metric v, HasLinearMap v, Typeable n, OrderedField n, Semigroup m)
--        => Subdiagram b v n m -> QDiagram v n m
-- getSub = subDia

-- replaceSub :: (QDiagram v n m -> QDiagram v n m) -> Subdiagram v n m -> QDiagram v n m
-- replaceSub f (Sub d r _) = r (f d)

------------------------------------------------------------------------
-- Subdiagram maps
------------------------------------------------------------------------

-- | A 'SubMap' is a map associating names to subdiagrams. There can
--   be multiple associations for any given name.
newtype SubMap v n m = SubMap (D.SubMap AName (DownAnnots v n) (UpAnnots v n m) (Annotation v n) (QDiaLeaf v n m))

-- instance Wrapped (SubMap b v n m) where
--   type Unwrapped (SubMap b v n m) = M.Map AName D.SubMap AName (DownAnnots v n) (UpAnnots v n m) (QDiaLeaf v n m)
--   _Wrapped' = iso (\(SubMap m) -> m) SubMap
-- instance Rewrapped (SubMap b v n m) (SubMap b' v' n' m')

type instance V (SubMap v n m) = v
type instance N (SubMap v n m) = n

-- instance Semigroup (SubMap b v n m) where
--   SubMap s1 <> SubMap s2 = SubMap (s1 <> s2)

-- | 'SubMap's form a monoid with the empty map as the identity, and
--   map union as the binary operation.  No information is ever lost:
--   if two maps have the same name in their domain, the resulting map
--   will associate that name to the concatenation of the information
--   associated with that name.
-- instance Monoid (SubMap b v n m) where
--   mempty  = SubMap M.empty
--   mappend = (<>)

-- instance (OrderedField n, Metric v, HasLinearMap v, Typeable n)
--       => HasOrigin (SubMap b v n m) where
--   moveOriginTo = over _Wrapped' . moveOriginTo

-- instance (OrderedField n, Metric v, HasLinearMap v, Typeable n)
--     => Transformable (SubMap b v n m) where
--   transform = over _Wrapped' . transform

-- | 'SubMap's are qualifiable: if @ns@ is a 'SubMap', then @a |>
--   ns@ is the same 'SubMap' except with every name qualified by
--   @a@.
-- instance Qualifiable (SubMap b v n m) where
--   a .>> SubMap m = SubMap $ M.mapKeys (a .>>) m

-- | A name acts on a name map by qualifying every name in it.
-- instance Action Name (SubMap b v n m) where
--   act = (.>>)

-- instance Action Name a => Action Name (Deletable a) where
--   act n (Deletable l a r) = Deletable l (act n a) r

-- Names do not act on other things.

-- instance Action Name (Query v n m)
-- instance Action Name (Envelope v n)
-- instance Action Name (Trace v n)

-- | Look for the given name in a name map, returning a list of
--   subdiagrams associated with that name.  If no names match the
--   given name exactly, return all the subdiagrams associated with
--   names of which the given name is a suffix.
-- lookupSub :: IsName nm => nm -> SubMap b v n m -> Maybe [Subdiagram b v n m]
-- lookupSub a (SubMap m)
--   = M.lookup n m `mplus`
--     (flattenNames . filter ((n `nameSuffixOf`) . fst) . M.assocs $ m)
--   where (Name n1) `nameSuffixOf` (Name n2) = n1 `isSuffixOf` n2
--         flattenNames [] = Nothing
--         flattenNames xs = Just . concatMap snd $ xs
--         n = toName a
