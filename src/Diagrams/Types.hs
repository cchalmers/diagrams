{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

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

    -- ** Basic type definitions
    QDiaLeaf(..) -- , withQDiaLeaf
  , QDiagram(..), Diagram

    -- * Operations on diagrams
    -- ** Creating diagrams
  , primQD, mkQD, mkQD', pointDiagram

    -- | For many more ways of combining diagrams, see
    --   "Diagrams.Combinators" and "Diagrams.TwoD.Combinators"
    --   from the diagrams-lib package.

    -- ** Modifying diagrams
    -- * Names
  , named
  , localize
  , styles

    -- *** Replaceing up annotations
  , modEnvelope
  , modTrace
  , upDiagram
  , upWith

    -- *** Adding static annotations
  , applyAnnot

    -- * Subdiagrams
  , SubDiagram
  , getSub
  , modSub
  , subLocation
  , allSubs

    -- * Subdiagram maps
  -- , SubMap
  -- , getSubMap
  -- , subLookup

    -- * Primtives
    -- $prim

  , Prim (..)
  , _Prim

    -- * Reexports
  , module Diagrams.Types.Annotations
  , module Diagrams.Types.Measure
  , module Diagrams.Types.Names
  , module Diagrams.Types.Style

  ) where

import           Control.Lens
import           Data.Coerce
import           Data.Semigroup
import           Data.Typeable

import           Data.Monoid.Coproduct
-- import           Data.Monoid.Deletable
import           Data.Monoid.WithSemigroup

-- import           Geometry.Align
import           Geometry.Envelope
import           Geometry.HasOrigin
import           Geometry.Juxtapose
import           Geometry.Points
import           Geometry.Query
import           Geometry.Space
import           Geometry.Trace
import           Geometry.Transform
import           Geometry.ThreeD.Shapes

import           Diagrams.Types.Annotations
import           Diagrams.Types.Measure
import           Diagrams.Types.Names
import           Diagrams.Types.Style
import qualified Diagrams.Types.Tree as T

import           Linear.Metric
import           Linear.V3 (V3)

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
data Prim where
  Prim :: Typeable p => p -> Prim

-- | Prism onto a 'Prim'.
_Prim :: Typeable p => Prism' Prim p
_Prim = prism' Prim (\(Prim p) -> cast p)
{-# INLINE _Prim #-}

-- | A leaf in a 'QDiagram' tree is either a 'Prim', or a \"delayed\"
--   @QDiagram@ which expands to a real @QDiagram@ once it learns the
--   \"final context\" in which it will be rendered.  For example, in
--   order to decide how to draw an arrow, we must know the precise
--   transformation applied to it (since the arrow head and tail are
--   scale-invariant).
data QDiaLeaf v m
  = PrimLeaf Prim
  | DelayedLeaf (DownAnnots v -> Double -> Double -> QDiagram v m)
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
newtype QDiagram v m = QD
  (T.IDUAL
    AName
    (DownAnnots v)
    (UpAnnots v m)
    (UpModify v)
    (Annotation v)
    (QDiaLeaf v m)
  )

instance Rewrapped (QDiagram v m) (QDiagram v' m')
instance Wrapped (QDiagram v m) where
  type Unwrapped (QDiagram v m) =
    T.IDUAL
      AName
      (DownAnnots v)
      (UpAnnots v m)
      (UpModify v)
      (Annotation v)
      (QDiaLeaf v m)
  _Wrapped' = coerced
  {-# INLINE _Wrapped' #-}

type instance V (QDiagram v m) = v
type instance N (QDiagram v m) = Double

-- | @Diagram v@ is a synonym for @'QDiagram' v Double Any@. That is,
--   the default sort of diagram is one where querying at a point
--   simply tells you whether the diagram contains that point or not.
--   Transforming a default diagram into one with a more interesting
--   query can be done via the 'Functor' instance of @'QDiagram' v n@ or
--   the 'value' function.
type Diagram v = QDiagram v Any

-- | Construct a diagram made up from an up annotation.
upDiagram :: UpAnnots v m -> QDiagram v m
upDiagram = QD . T.leafU
{-# INLINE upDiagram #-}

-- | Construct a 'upDiagram' by apply a function to the empty up
--   annotations.
upWith :: Monoid m => (UpAnnots v m -> UpAnnots v m) -> QDiagram v m
upWith f = upDiagram (f emptyUp)
{-# INLINE upWith #-}

-- | Create a \"point diagram\", which has no content, no trace, an
--   empty query, and a point envelope.
pointDiagram
  :: (Metric v, Monoid m)
  => Point v Double
  -> QDiagram v m
pointDiagram p = upWith $ upEnvelope .~ pointEnvelope p
{-# INLINE pointDiagram #-}

-- | Modify the envelope. (Are there laws we need to satisfy?)
modEnvelope :: (Envelope v Double -> Envelope v Double) -> QDiagram v m -> QDiagram v m
modEnvelope f = over _Wrapped' $ T.modU (EnvMod f)
{-# INLINE modEnvelope #-}

-- | Modify the trace. (Are there laws we need to satisfy?)
modTrace :: (Trace v Double -> Trace v Double) -> QDiagram v m -> QDiagram v m
modTrace f = over _Wrapped' $ T.modU (TraceMod f)
{-# INLINE modTrace #-}

-- | Apply an annotation.
applyAnnot :: AnnotationSpace a v => AReview a r -> r -> QDiagram v m -> QDiagram v m
applyAnnot l r = over _Wrapped' $ T.annot (mkAnnot l r)
{-# INLINE applyAnnot #-}

-- | Traversal over all subdiagrams labeled with all 'AName's in the
--   name.
named
  :: (IsName nm, HasLinearMap v, Semigroup m)
  => nm -> Traversal' (QDiagram v m) (QDiagram v m)
named (toName -> Name ns) = _Wrapped' . T.traverseSub ns . _Unwrapped'

-- | Traversal over the styles of each leaf.
styles :: HasLinearMap v => Traversal' (QDiagram v m) (Style v)
styles = _Wrapped . T.downs . downStyle

-- | Get a list of names of subdiagrams and their locations.
-- names :: (Metric v, HasLinearMap v, Typeable n, Semigroup m, OrderedField n)
--       => QDiagram v m -> [(Name, [Point v n])]
-- names = (map . second . map) location . M.assocs . view (subMap . _Wrapped')

-- | Lookup the most recent diagram associated with (some
--   qualification of) the given name.
-- lookupName :: (IsName nm, Metric v, HasLinearMap v, Typeable n, Semigroup m, OrderedField n)
--            => nm -> QDiagram v m -> Maybe (Subdiagram b v m)
-- lookupName n d = lookupSub (toName n) (d^.subMap) >>= listToMaybe

-- | \"Localize\" a diagram by hiding all the names, so they are no
--   longer visible to the outside.
localize :: QDiagram v m -> QDiagram v m
localize = over _Wrapped' T.resetLabels

-- | Make a QDiagram using the default envelope, trace and query for the
--   primitive.
primQD
  :: (InSpace v Double a, Typeable a, Enveloped a, Traced a, HasQuery a m)
  => a
  -> QDiagram v m
primQD a = mkQD (Prim a) (getEnvelope a) (getTrace a) (getQuery a)
{-# INLINE primQD #-}

-- | Create a diagram from a single primitive, along with an envelope,
--   trace, subdiagram map, and query function.
mkQD :: Prim -> Envelope v Double -> Trace v Double -> Query v Double m -> QDiagram v m
mkQD p = mkQD' (PrimLeaf p)
{-# INLINE mkQD #-}

-- | Create a diagram from a generic QDiaLeaf, along with an envelope,
--   trace, subdiagram map, and query function.
mkQD'
  :: QDiaLeaf v m
  -> Envelope v Double
  -> Trace v Double
  -> Query v Double m
  -> QDiagram v m
mkQD' l e t q = QD $ T.leaf (UpAnnots e t q) l
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
instance Monoid (QDiagram v m) where
  mempty  = QD mempty
  {-# INLINE mempty  #-}
  mappend = (<>)
  {-# INLINE mappend #-}

instance Semigroup (QDiagram v m) where
  QD d1 <> QD d2 = QD (d2 <> d1)
  {-# INLINE (<>) #-}
    -- swap order so that primitives of d2 come first, i.e. will be
    -- rendered first, i.e. will be on the bottom.

instance Functor (QDiagram v) where
  fmap f (QD d) = QD $ T.mapUAL (fmap f) id (fmap f) d
  {-# INLINE fmap #-}

instance HasLinearMap v => ApplyStyle (QDiagram v m) where
  applyStyle = over _Wrapped' . T.down . inR
  {-# INLINE applyStyle #-}

instance (HasLinearMap v, Monoid' m)
    => HasQuery (QDiagram v m) m where
  getQuery = foldU (\u e -> view upQuery u <> e) mempty
  {-# INLINE getQuery #-}

instance (Metric v, HasLinearMap v)
    => Juxtaposable (QDiagram v m) where
  juxtapose = juxtaposeDefault
  {-# INLINE juxtapose #-}

instance HasLinearMap v => Enveloped (QDiagram v m) where
  getEnvelope = foldU (\u e -> view upEnvelope u <> e) EmptyEnvelope
  {-# INLINE getEnvelope #-}

-- | Fold over all up annotation in a diagram.
foldU :: HasLinearMap v => (UpAnnots v m -> b -> b) -> b -> QDiagram v m -> b
foldU f b0 (QD t) = T.foldU f b0 t

instance HasLinearMap v => Traced (QDiagram v m) where
  getTrace = foldU (\u e -> view upTrace u <> e) mempty
  {-# INLINE getTrace #-}

instance (Metric v, HasLinearMap v) => HasOrigin (QDiagram v m) where
  moveOriginTo = translate . (origin .-.)
  {-# INLINE moveOriginTo #-}

instance Transformable (QDiagram v m) where
  transform = over _Wrapped' . T.down . inL
  {-# INLINE transform #-}

-- instance (HasLinearMap v, OrderedField n)
--     => Alignable (QDiagram v m) where
--   defaultBoundary = envelopeBoundary
--   {-# INLINE defaultBoundary #-}

instance Qualifiable (QDiagram v m) where
  (toName -> Name nms) .>> QD t = QD (T.labels nms t)
  {-# INLINE (.>>) #-}

instance CuboidLike (QDiagram V3 Any) where
  cube = primQD Cube

instance EllipsoidLike (QDiagram V3 Any) where
  sphere = primQD Sphere

instance FrustumLike (QDiagram V3 Any) where
  frustum a b = primQD (Frustum a b)

------------------------------------------------------------
--  Subdiagrams
------------------------------------------------------------

-- | A @Subdiagram@ represents a diagram embedded within the context
--   of a larger diagram.  Essentially, it consists of a diagram
--   paired with any accumulated information from the larger context
--   (transformations, attributes, etc.).
newtype SubDiagram v m = SubDiagram
  (T.SubIDUAL
    AName
    (DownAnnots v)
    (UpAnnots v m)
    (UpModify v)
    (Annotation v)
    (QDiaLeaf v m)
  )

type instance V (SubDiagram v m) = v
type instance N (SubDiagram v m) = Double

-- | Turn a diagram into a subdiagram with no accumulated context.
-- mkSubdiagram :: QDiagram v m -> Subdiagram v m
-- mkSubdiagram d = Subdiagram d id id empty

-- | Get the location of a subdiagram; that is, the location of its
--   local origin /with respect to/ the vector space of its parent
--   diagram.  In other words, the point where its local origin
--   \"ended up\".
subLocation
  :: HasLinearMap v => SubDiagram v m -> Point v Double
subLocation (SubDiagram sub) = case T.accumDown sub of
  Just d  -> papply (killR d) origin
  Nothing -> origin
{-# INLINE subLocation #-}

-- | Turn a subdiagram into a normal diagram, including the enclosing
--   context.  Concretely, a subdiagram is a pair of (1) a diagram and
--   (2) a \"context\" consisting of an extra transformation and
--   attributes.  @getSub@ simply applies the transformation and
--   attributes to the diagram to get the corresponding \"top-level\"
--   diagram.
getSub :: SubDiagram v m -> QDiagram v m
getSub (SubDiagram sub) = QD (T.subPos sub)

-- | Return all named subdiagrams with the names attacted to them.
allSubs
  :: (HasLinearMap v, Monoid' m)
  => QDiagram v m -> [(Name, SubDiagram v m)]
allSubs (QD t) = map (\(is,st) -> (Name is, SubDiagram st)) (T.allSubs t)

-- | Return the full diagram with the subdiagram modified.
modSub
  :: (QDiagram v m -> QDiagram v m)
  -> SubDiagram v m
  -> QDiagram v m
modSub f (SubDiagram sub) = QD $ T.subPeek sub $ coerce f (T.subPos sub)

------------------------------------------------------------------------
-- Subdiagram maps
------------------------------------------------------------------------

-- | A 'SubMap' is a map associating names to subdiagrams. There can
--   be multiple associations for any given name.
-- newtype SubMap v m = SubMap
--   (T.SubMap
--     AName
--     (DownAnnots v)
--     (UpAnnots v m)
--     (Annotation v)
--     (QDiaLeaf v m)
--   )

-- instance Wrapped (SubMap b v m) where
--   type Unwrapped (SubMap b v m) = M.Map AName T.SubMap AName (DownAnnots v) (UpAnnots v m) (QDiaLeaf v m)
--   _Wrapped' = iso (\(SubMap m) -> m) SubMap
-- instance Rewrapped (SubMap b v m) (SubMap b' v' n' m')

-- type instance V (SubMap v m) = v
-- type instance N (SubMap v m) = n

-- getSubMap :: (HasLinearMap v, OrderedField n, Monoid' m) => QDiagram v m -> SubMap v m
-- getSubMap (QD d) = SubMap (T.getSubMap d)

-- subLookup :: IsName nm => nm -> SubMap v m -> [SubDiagram v m]
-- subLookup (toName -> Name nms) (SubMap m) = coerce $ T.lookupSub nms m

-- | Look for the given name in a name map, returning a list of
--   subdiagrams associated with that name.  If no names match the
--   given name exactly, return all the subdiagrams associated with
--   names of which the given name is a suffix.
-- lookupSub :: IsName nm => nm -> SubMap b v m -> Maybe [Subdiagram b v m]
-- lookupSub a (SubMap m)
--   = M.lookup n m `mplus`
--     (flattenNames . filter ((n `nameSuffixOf`) . fst) . M.assocs $ m)
--   where (Name n1) `nameSuffixOf` (Name n2) = n1 `isSuffixOf` n2
--         flattenNames [] = Nothing
--         flattenNames xs = Just . concatMap snd $ xs
--         n = toName a
