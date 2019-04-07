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
  , primQD, mkQD, mkQD', mkQDU, pointDiagram

    -- * Path primitive
  , strokePath, strokePathCrossings

    -- | For many more ways of combining diagrams, see
    --   "Diagrams.Combinators" and "Diagrams.TwoD.Combinators"
    --   from the diagrams-lib package.

    -- ** Modifying diagrams
    -- * Names
  , named
  , localize
  , styles
  , leafs
  , releaf
  , down

    -- *** Replaceing up annotations
  , modEnvelope
  , replaceEnvelope
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
  , findSubs

    -- * Subdiagram maps
  -- , SubMap
  -- , getSubMap
  -- , subLookup

    -- * Primtives
    -- $prim

  , Prim (..)
  , _Prim

    -- ** Number classes
  , TypeableFloat

    -- * Reexports
  , module Diagrams.Types.Annotations
  , module Diagrams.Types.Measure
  , module Diagrams.Types.Names
  , module Diagrams.Types.Style

  ) where

import           Control.Lens
import           Data.Coerce
import           Data.Maybe
import           Data.Monoid.Coproduct.Strict
import           Data.Monoid.WithSemigroup
import           Data.Semigroup
import           Data.Typeable

import           Geometry.Envelope
import           Geometry.HasOrigin
import           Geometry.Juxtapose
import           Geometry.Path                (Path, toPath)
import           Geometry.Points
import           Geometry.Query
import           Geometry.Segment             (Crossings)
import           Geometry.Space
import           Geometry.ThreeD.Shapes
import           Geometry.Trace
import           Geometry.Trail               (FromTrail (..))
import           Geometry.Transform

import           Diagrams.Types.Annotations
import           Diagrams.Types.Measure
import           Diagrams.Types.Names
import           Diagrams.Types.Style
import qualified Diagrams.Types.Tree          as T

import           Linear.Metric
import           Linear.V2                    (V2)
import           Linear.V3                    (V3)
import           Linear.Vector

-- | Class of numbers that are 'RealFloat' and 'Typeable'. This class is used to
--   shorten type constraints.
class (Typeable n, RealFloat n) => TypeableFloat n
instance (Typeable n, RealFloat n) => TypeableFloat n
-- use class instead of type constraint so users don't need constraint kinds pragma

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
newtype QDiagram v n m = QD (QDT v n m)

type QDT v n m =
  T.IDUAL
    AName
    (DownAnnots v n)
    (UpAnnots v n m)
    (UpModify v n)
    (Annotation v n)
    (QDiaLeaf v n m)

instance Rewrapped (QDiagram v n m) (QDiagram v' n' m')
instance Wrapped (QDiagram v n m) where
  type Unwrapped (QDiagram v n m) = QDT v n m
  _Wrapped' = coerced
  {-# INLINE _Wrapped' #-}

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
upDiagram = QD . T.leafU
{-# INLINE upDiagram #-}

-- | Construct a 'upDiagram' by apply a function to the empty up
--   annotations.
upWith :: Monoid m => (UpAnnots v n m -> UpAnnots v n m) -> QDiagram v n m
upWith f = upDiagram (f emptyUp)
{-# INLINE upWith #-}

down
  :: forall v n m
  . (Traversable v, Additive v, Floating n)
    => DownAnnots v n -> QDiagram v n m -> QDiagram v n m
down = coerce (T.down :: DownAnnots v n -> QDT v n m -> QDT v n m)

-- | Create a \"point diagram\", which has no content, no trace, an
--   empty query, and a point envelope.
pointDiagram
  :: (Metric v, Fractional n, Monoid m)
  => Point v n
  -> QDiagram v n m
pointDiagram p = upWith $ upEnvelope .~ pointEnvelope p
{-# INLINE pointDiagram #-}

-- | Modify the envelope. (Are there laws we need to satisfy?)
modEnvelope
  :: (Envelope v n -> Envelope v n) -> QDiagram v n m -> QDiagram v n m
modEnvelope f = over _Wrapped' $ T.modU (EnvMod f)
{-# INLINE modEnvelope #-}

-- | Modify the envelope. (Are there laws we need to satisfy?)
replaceEnvelope
  :: Envelope v n -> QDiagram v n m -> QDiagram v n m
replaceEnvelope e = over _Wrapped' $ T.modU (EnvReplace e)
{-# INLINE replaceEnvelope #-}

-- | Modify the trace. (Are there laws we need to satisfy?)
modTrace :: (Trace v n -> Trace v n) -> QDiagram v n m -> QDiagram v n m
modTrace f = over _Wrapped' $ T.modU (TraceMod f)
{-# INLINE modTrace #-}

-- | Apply an annotation.
applyAnnot :: AnnotationSpace a v n => AReview a r -> r -> QDiagram v n m -> QDiagram v n m
applyAnnot l r = over _Wrapped' $ T.annot (mkAnnot l r)
{-# INLINE applyAnnot #-}

-- | Traversal over all subdiagrams labeled with all 'AName's in the
--   name.
named
  :: (IsName nm, HasLinearMap v, OrderedField n, Semigroup m)
  => nm -> Traversal' (QDiagram v n m) (QDiagram v n m)
named (toName -> Name ns) = _Wrapped' . T.traverseSub ns . _Unwrapped'

-- | Traversal over the styles of each leaf.
styles
  :: (HasLinearMap v, OrderedField n)
  => Traversal' (QDiagram v n m) (Style v n)
styles = _Wrapped . T.downs . downStyle

leafs
  :: (HasLinearMap v, OrderedField n)
  => Traversal' (QDiagram v n m) (QDiagram v n m)
-- leafs = coerced . (T.leafs :: Traversal' (QDT v n m) (QDT v n m)) . coerced
leafs = _Wrapped' . T.leafs . _Unwrapped
{-# INLINE leafs #-}

releaf
  :: forall v n m. (HasLinearMap v, OrderedField n)
  => (DownAnnots v n -> UpAnnots v n m -> QDiaLeaf v n m -> QDiagram v n m)
  -> QDiagram v n m
  -> QDiagram v n m
releaf = coerce (T.releaf :: ((DownAnnots v n -> UpAnnots v n m  -> QDiaLeaf v n m -> QDT v n m) -> QDT v n m -> QDT v n m))
{-# INLINE releaf #-}

-- | Find all SubDiagrams that match the given name.
findSubs
  :: (IsName nm, HasLinearMap v, OrderedField n, Monoid' m)
  => nm -> QDiagram v n m -> [SubDiagram v n m]
findSubs (toName -> Name nm) (QD t) = coerce (T.findSubs nm t)

-- | Get a list of names of subdiagrams and their locations.
-- names :: (Metric v, HasLinearMap v, Typeable n, Semigroup m, OrderedField n)
--       => QDiagram v n m -> [(Name, [Point v n])]
-- names = (map . second . map) location . M.assocs . view (subMap . _Wrapped')

-- | Lookup the most recent diagram associated with (some
--   qualification of) the given name.
-- lookupName :: (IsName nm, Metric v, HasLinearMap v, Typeable n, Semigroup m, OrderedField n)
--            => nm -> QDiagram v n m -> Maybe (Subdiagram b v n m)
-- lookupName n d = lookupSub (toName n) (d^.subMap) >>= listToMaybe

-- | \"Localize\" a diagram by hiding all the names, so they are no
--   longer visible to the outside.
localize :: QDiagram v n m -> QDiagram v n m
localize = over _Wrapped' T.resetLabels

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
mkQD' l e t q = mkQDU l (UpAnnots e t q)
{-# INLINE mkQD' #-}

mkQDU :: QDiaLeaf v n m -> UpAnnots v n m -> QDiagram v n m
mkQDU l u = QD $ T.leaf u l

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
instance Monoid (QDiagram v n m) where
  mempty  = QD mempty
  {-# INLINE mempty  #-}
  mappend = (<>)
  {-# INLINE mappend #-}

instance Semigroup (QDiagram v n m) where
  QD d1 <> QD d2 = QD (d2 <> d1)
  {-# NOINLINE (<>) #-}
  -- Swap order so that primitives of d2 come first, i.e. will be
  -- rendered first, i.e. will be on the bottom.
  -- Do NOT inline this, it's huge

instance Functor (QDiagram v n) where
  fmap = \f (QD d) -> QD $ T.mapUAL (fmap f) id (fmap f) d
  {-# INLINE fmap #-}

applyStyleDia :: (HasLinearMap v, Floating n) => Style v n -> QDiagram v n m -> QDiagram v n m
applyStyleDia = over _Wrapped' . T.down . inR
{-# SPECIALISE applyStyleDia :: Style V2 Double -> QDiagram V2 Double m -> QDiagram V2 Double m #-}
{-# SPECIALISE applyStyleDia :: Style V3 Double -> QDiagram V3 Double m -> QDiagram V3 Double m #-}

instance (HasLinearMap v, Floating n) => ApplyStyle (QDiagram v n m) where
  applyStyle = applyStyleDia -- over _Wrapped' . T.down . inR
  {-# INLINE applyStyle #-}

getQueryDia :: (HasLinearMap v, OrderedField n, Monoid m) => QDiagram v n m -> Query v n m
getQueryDia = foldU (\u e -> view upQuery u `mappend` e) mempty
{-# SPECIALISE getQueryDia :: Diagram V2 -> Query V2 Double Any #-}
{-# SPECIALISE getQueryDia :: Diagram V3 -> Query V3 Double Any #-}

instance (HasLinearMap v, OrderedField n, Monoid' m)
    => HasQuery (QDiagram v n m) m where
  getQuery = getQueryDia
  {-# INLINE getQuery #-}

juxtaposeDia
  :: (Metric v, HasLinearMap v, OrderedField n)
  => v n -> QDiagram v n m -> QDiagram v n m -> QDiagram v n m
juxtaposeDia = juxtaposeDefault
{-# SPECIALISE juxtaposeDia :: V2 Double -> QDiagram V2 Double m -> QDiagram V2 Double m -> QDiagram V2 Double m #-}
{-# SPECIALISE juxtaposeDia :: V3 Double -> QDiagram V3 Double m -> QDiagram V3 Double m -> QDiagram V3 Double m #-}

instance (Metric v, HasLinearMap v, OrderedField n)
    => Juxtaposable (QDiagram v n m) where
  juxtapose = juxtaposeDia
  {-# INLINE juxtapose #-}

getEnvelopeDia :: (HasLinearMap v, OrderedField n) => QDiagram v n m -> Envelope v n
getEnvelopeDia = fromMaybe EmptyEnvelope . T.getU . T.mapUAL (view upEnvelope) id id . (\(QD t) -> t)
{-# SPECIALISE getEnvelopeDia :: QDiagram V2 Double m -> Envelope V2 Double #-}
{-# SPECIALISE getEnvelopeDia :: QDiagram V3 Double m -> Envelope V3 Double #-}

instance (HasLinearMap v, OrderedField n) => Enveloped (QDiagram v n m) where
  getEnvelope = getEnvelopeDia
  {-# INLINE getEnvelope #-}

-- | 'stroke' specialised to 'Path'.
strokePath :: TypeableFloat n => Path V2 n -> QDiagram V2 n Any
strokePath = \p -> mkQD (Prim p) (getEnvelope p) (getTrace p) (Any . (/= 0) <$> getQuery p)
{-# SPECIALISE strokePath :: Path V2 Double -> Diagram V2 #-}

strokePathCrossings :: TypeableFloat n => Path V2 n -> QDiagram V2 n Crossings
strokePathCrossings = primQD
{-# SPECIALISE strokePathCrossings :: Path V2 Double -> QDiagram V2 Double Crossings #-}

instance TypeableFloat n => FromTrail (QDiagram V2 n Any) where
  fromLocTrail = strokePath . toPath
  {-# INLINE fromLocTrail #-}

instance TypeableFloat n => FromTrail (QDiagram V2 n Crossings) where
  fromLocTrail = strokePathCrossings . toPath
  {-# INLINE fromLocTrail #-}

-- | Fold over all up annotation in a diagram.
foldU
  :: (HasLinearMap v, OrderedField n)
  => (UpAnnots v n m -> b -> b) -> b -> QDiagram v n m -> b
foldU = \f b0 (QD t) -> T.foldU f b0 t
{-# INLINE foldU #-}

getTraceDia :: (HasLinearMap v, OrderedField n) => QDiagram v n m -> Trace v n
getTraceDia = foldU (\u e -> view upTrace u <> e) mempty
{-# SPECIALISE getTraceDia :: QDiagram V2 Double m -> Trace V2 Double #-}
{-# SPECIALISE getTraceDia :: QDiagram V3 Double m -> Trace V3 Double #-}

instance (HasLinearMap v, OrderedField n) => Traced (QDiagram v n m) where
  getTrace = getTraceDia
  {-# INLINE getTrace #-}

instance (Metric v, HasLinearMap v, OrderedField n)
    => HasOrigin (QDiagram v n m) where
  moveOriginTo = translate . (origin .-.)
  {-# INLINE moveOriginTo #-}

transformDia :: (Traversable v, Additive v, Floating n) => Transformation v n -> QDiagram v n m -> QDiagram v n m
transformDia = over _Wrapped' . T.down . inL
{-# SPECIALISE transformDia :: Transformation V2 Double -> QDiagram V2 Double m ->
   QDiagram V2 Double m #-}

instance (Additive v, Traversable v, Floating n) => Transformable (QDiagram v n m) where
  transform = transformDia
  {-# INLINE transform #-}

-- instance (HasLinearMap v, OrderedField n)
--     => Alignable (QDiagram v n m) where
--   defaultBoundary = envelopeBoundary
--   {-# INLINE defaultBoundary #-}

instance Qualifiable (QDiagram v n m) where
  (toName -> Name [])  .>> d    = d
  (toName -> Name nms) .>> QD t = QD (T.labels nms t)
  {-# INLINE (.>>) #-}

instance (OrderedField n, Typeable n) => CuboidLike (QDiagram V3 n Any) where
  cube = primQD Cube

instance (OrderedField n, Typeable n) => EllipsoidLike (QDiagram V3 n Any) where
  sphere = primQD Sphere

instance TypeableFloat n => FrustumLike (QDiagram V3 n Any) where
  frustum a b = primQD (Frustum a b)

------------------------------------------------------------
--  Subdiagrams
------------------------------------------------------------

-- | A @Subdiagram@ represents a diagram embedded within the context
--   of a larger diagram.  Essentially, it consists of a diagram
--   paired with any accumulated information from the larger context
--   (transformations, attributes, etc.).
newtype SubDiagram v n m = SubDiagram
  (T.SubIDUAL
    AName
    (DownAnnots v n)
    (UpAnnots v n m)
    (UpModify v n)
    (Annotation v n)
    (QDiaLeaf v n m)
  )

type instance V (SubDiagram v n m) = v
type instance N (SubDiagram v n m) = n

-- | Turn a diagram into a subdiagram with no accumulated context.
-- mkSubdiagram :: QDiagram v n m -> Subdiagram v n m
-- mkSubdiagram d = Subdiagram d id id empty

-- | Get the location of a subdiagram; that is, the location of its
--   local origin /with respect to/ the vector space of its parent
--   diagram.  In other words, the point where its local origin
--   \"ended up\".
subLocation
  :: (HasLinearMap v, Num n)
  => SubDiagram v n m -> Point v n
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
getSub :: SubDiagram v n m -> QDiagram v n m
getSub (SubDiagram sub) = QD (T.subPos sub)

-- | Return all named subdiagrams with the names attacted to them.
allSubs
  :: (HasLinearMap v, OrderedField n, Monoid' m)
  => QDiagram v n m -> [(Name, SubDiagram v n m)]
allSubs (QD t) = map (\(is,st) -> (Name is, SubDiagram st)) (T.allSubs t)

-- | Return the full diagram with the subdiagram modified.
modSub
  :: (QDiagram v n m -> QDiagram v n m)
  -> SubDiagram v n m
  -> QDiagram v n m
modSub f (SubDiagram sub) = QD $ T.subPeek sub $ coerce f (T.subPos sub)

------------------------------------------------------------------------
-- Subdiagram maps
------------------------------------------------------------------------

-- | A 'SubMap' is a map associating names to subdiagrams. There can
--   be multiple associations for any given name.
-- newtype SubMap v n m = SubMap
--   (T.SubMap
--     AName
--     (DownAnnots v n)
--     (UpAnnots v n m)
--     (Annotation v n)
--     (QDiaLeaf v n m)
--   )

-- instance Wrapped (SubMap b v n m) where
--   type Unwrapped (SubMap b v n m) = M.Map AName T.SubMap AName (DownAnnots v n) (UpAnnots v n m) (QDiaLeaf v n m)
--   _Wrapped' = iso (\(SubMap m) -> m) SubMap
-- instance Rewrapped (SubMap b v n m) (SubMap b' v' n' m')

-- type instance V (SubMap v n m) = v
-- type instance N (SubMap v n m) = n

-- getSubMap :: (HasLinearMap v, OrderedField n, Monoid' m) => QDiagram v n m -> SubMap v n m
-- getSubMap (QD d) = SubMap (T.getSubMap d)

-- subLookup :: IsName nm => nm -> SubMap v n m -> [SubDiagram v n m]
-- subLookup (toName -> Name nms) (SubMap m) = coerce $ T.lookupSub nms m

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
