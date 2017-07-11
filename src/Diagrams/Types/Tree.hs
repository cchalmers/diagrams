{-# LANGUAGE CPP                   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Types.Tree
-- Copyright   :  (c) 2016 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Diagrams are made of a custom tree type with the following features:
--
--   * O(1) transforms and style applications
--   * the ability to traverse named subdiagrams
--
-----------------------------------------------------------------------------

module Diagrams.Types.Tree
  (
    -- * DUAL-trees
    NE(..), IDUAL(..)

    -- * Constructing DUAL-trees
  , leaf, leafU, modU, down, annot

    -- * Folding DUAL-trees
  , foldDUAL

  -- * Labels
  , label
  , labels
  , resetLabels
  , downs

    -- * Up annotations
  -- , _u
  , getU
  , foldU
  , foldUl'
  , gu
  , mapU
  , mapUAL
  -- , preapplyU
  -- , postapplyU

    -- * Subtree
  , SubIDUAL (..)
  , sub
  , subPeeks
  , allSubs

    -- * Submap
  -- , SubMap
  -- , getSubMap
  -- , lookupSub
  , traverseSub
  , leafs
  , tapeMatches
  , matchingU
  -- , ixDUAL

    -- * Debugging
  , Tape (..)
  , startTape
  , path
  , nannots
  , getI
  , idualShow
  , pushDown
  ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
-- import           Control.DeepSeq
import           Data.Foldable             as F (foldMap)
import           Data.Monoid.Action
import           Data.Monoid.WithSemigroup
import           Data.Semigroup
import           Data.Typeable
import Data.Foldable

import Data.Hashable (Hashable)
import           Data.HashMap.Lazy                 (HashMap)
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.HashMap.Lazy                  as HM
import           Data.Sequence             (Seq)
import qualified Data.Sequence             as Seq
import           Data.Set                  (Set)
import qualified Data.Set                  as Set

import           Control.Lens
import           Control.Lens.Internal (Pretext (..), Pretext', ipeek)
import           Data.Profunctor.Unsafe ((#.))

fldUl' :: (Action d u, Action m u) => (b -> u -> b) -> b -> NE i d u m a l -> b
fldUl' = go where
  go f !b = \case
    Leaf     u _         -> f b u
    Up       u           -> f b u
    UpMod  _ m t         -> go (\b' u -> f b' $! act m u) b t
    Label  _ _ (NE t)    -> go f b t
    Label  _ _ EmptyDUAL -> b
    Down   _ d t         -> go (\b' u -> f b' $! act d u) b t
    Annot  _ _ t         -> go f b t
    Concat _ ts          -> foldl' (\b' t -> go f b' t) b ts
{-# INLINE fldUl' #-}

-- | Fold each u annotation in order.
foldUl' :: (Action d u, Action m u) => (b -> u -> b) -> b -> IDUAL i d u m a l -> b
foldUl' = \f b0 -> \case
  NE t      -> fldUl' f b0 t
  EmptyDUAL -> b0
{-# INLINE foldUl' #-}

------------------------------------------------------------------------
-- Labeling
------------------------------------------------------------------------

-- Tapes ---------------------------------------------------------------

-- | A tape is a unique path to part to a sub-DUALTree.
data Tape = T [Int] Int
  -- Internal representation:
  --   - path to sub-DUALTree, representated by index of each Concat node
  --   - number of static annotations to pass after following the path
  deriving (Show, Eq, Ord)

startTape :: Tape
startTape = T [] 0
{-# INLINE startTape #-}

-- | The indexes of the concat nodes that point to a anno
path :: Lens' Tape [Int]
path f (T p n) = f p <&> \p' -> T p' n
{-# INLINE path #-}

-- | Number of annotation to pass between the last concat and the
--   target.
nannots :: Lens' Tape Int
nannots f (T p n) = f n <&> T p

-- Routes --------------------------------------------------------------

-- | The route to take while traversing an idual tree. Consists of two
--   parts:
--     - Subtrees to target before the next Concat. This is represented
--       by the number of Annotation nodes to pass.
--     - Targets for next concat together with the route for that
--       concat.
data Route = Route [Int] [(Int,Route)]
  deriving Show

-- equating :: Eq b => (a -> b) -> a -> a -> Bool
-- equating f a b = f a == f b

-- testTapes :: [Tape]
-- testTapes = [T [] 0, T [0] 0, T [0,0] 0, T [0,1] 0, T [1] 0, T [1] 1]

-- | Construct a route to traverse an idual tree. Expects the tapes to
--   be given in order.
mkRoute :: [Tape] -> Route
mkRoute = go where
  -- collect all targets before the next concat
  go :: [Tape] -> Route
  go (T [] n:ts) = let Route ns its = go ts
                   in  Route (n:ns) its
  go ts          = Route [] (go2 ts)

  -- group together non-empty paths with their next concat index
  go2 :: [Tape] -> [(Int, Route)]
  go2 (T (i:is) n:ts) = let (matched,ts') = go3 i ts
                        in  (i, mkRoute $ T is n : matched) : go2 ts'
  go2 _               = []

  -- find subsequent tapes that follow the same concat index
  go3 :: Int -> [Tape] -> ([Tape], [Tape])
  go3 i ((T (i':is) n):ts')
    | i == i' = let (matched, ts'') = go3 i ts'
                in  (T is n:matched, ts'')
  go3 _ ts'' = ([], ts'')

-- Labels --------------------------------------------------------------

-- Label map -----------------------------------------------------------

-- | A map from indexes to a set of subtrees.
data Labels i
  = Labels (HashMap i (Set Tape))
  | NoLabels
  deriving (Show, Eq)

combineMaps
  :: (Hashable i, Eq i)
  => Int -- ^ length on concat on left (-1 for non-concat)
  -> Labels i -- ^ index map on left
  -> Int  -- ^ length on concat on left (-1 for non-concat)
  -> Labels i -- ^ index map on right
  -> Labels i
combineMaps i1 l1 i2 l2 =
  case (l1,l2) of
    (Labels m1 , Labels m2) -> Labels $ HM.unionWith Set.union (g f1 m1) (g f2 m2)
    (Labels m1 , NoLabels)  -> Labels $ g f1 m1
    (NoLabels  , Labels m2) -> Labels $ g f2 m2
    (NoLabels  , NoLabels)  -> NoLabels
  where
  -- This alters the tapes into the subdiagrams according to how the
  -- DUALTree combines nodes:
  --   - If either of the top level nodes not 'Concat', the top level
  --     node will become a 'Concat' of length 2, containing either
  --     side.
  --   - If they are both `Concat`, combine them into a new `Concat`
  f1 | i1 < 0    = (0:)
     | otherwise = id
  f2 | i2 < 0    = (max 1 i1:)
     | otherwise = _head +~ max 1 i1
  g f = HM.map (Set.map $ path %~ f)
{-# INLINE combineMaps #-}

------------------------------------------------------------------------
-- Non-Empty tree
------------------------------------------------------------------------

-- | Non-Empty DUAL-tree. u annotations are included so parts of the
--   tree can be edited and the u annotations can be rebuilt properly.
--   The tree is non-empty in the sence it always has at least a @u@
--   annotation. It doesn't not have to contain any leafs.
--
--   Invarients:
--     - The 'Seq' in 'Concat' has at least two elements
--     - The @u@ annotations for Down, Annot and Concat are only caches,
--       they should be equal to the product of the up annotations below
--       them
--     - There should be no observable difference (other than
--       performance) from having the down annotation "pushed".
data NE i d u m a l
  = Leaf   u !l                                     -- ^ @l@eaf
  | Up     u                                        -- ^ @u@p
  | UpMod  !(Labels i) m !(NE i d u m a l)            -- ^ up-@m@od annotation
  | Label  !(Labels i) (Maybe d) !(IDUAL i d u m a l) -- ^ label annotation
  | Down   !(Labels i) !d !(NE i d u m a l)           -- ^ @d@own annotation
  | Annot  !(Labels i) !a !(NE i d u m a l)           -- ^ @a@nnotation
  | Concat !(Labels i) (Seq (NE i d u m a l))         -- ^ n-way branch
  deriving (Typeable, Functor, Foldable, Traversable)

-- For now I've kept the type annotation instead of using the actual
-- types from "Diagrams.Types.Annotations" because it's easier to debug
-- and reason about.

-- XXX The reason for the UpMod function is to get `pad`, `frame` and
-- friends to place nice with subdiagram traversals. Right now, as soon
-- as you use any of these envelope modifying functions you lose the
-- ability to rebuild the envelope when editing a subdiagram. (Same for
-- trace).

gu :: (Action d u, Action m u, Monoid u) => NE i d u m a l -> u
gu = go where
  go = \case
    Leaf     u _         -> u
    Up       u           -> u
    UpMod  _ m t         -> act m (go t)
    Label  _ _ (NE t)    -> go t
    Label  _ _ EmptyDUAL -> mempty
    Down   _ d t         -> act d (go t)
    Annot  _ _ t         -> go t
    Concat _ ts          -> foldMap go ts
{-# INLINE gu #-}

fldU :: (Action d u, Action m u) => (u -> b -> b) -> b -> NE i d u m a l -> b
fldU f = go where
  go b = \case
    Leaf     u _         -> f u b
    Up       u           -> f u b
    UpMod  _ m t         -> fldU (f . act m) b t
    Label  _ _ (NE t)    -> go b t
    Label  _ _ EmptyDUAL -> b
    Down   _ d t         -> fldU (f . act d) b t
    Annot  _ _ t         -> go b t
    Concat _ ts          -> foldr (\t b' -> go b' t) b ts
{-# INLINE fldU #-}

gi :: NE i d u m a l -> Labels i
gi = \case
  Leaf     _ _ -> NoLabels
  Up       _   -> NoLabels
  UpMod  i _ _ -> i
  Label  i _ _ -> i
  Down   i _ _ -> i
  Annot  i _ _ -> i
  Concat i _   -> i
{-# INLINE gi #-}

fual :: (u -> u') -> (a -> a') -> (l -> l') -> NE i d u m a l -> NE i d u' m a' l'
fual uf af lf = go where
  go = \case
    Leaf u l            -> Leaf (uf u) (lf l)
    Up   u              -> Up (uf u)
    UpMod i m t         -> UpMod i m (go t)
    Label i d EmptyDUAL -> Label i d EmptyDUAL
    Label i d (NE t)    -> Label i d (NE (go t))
    Down i d t          -> Down i d (go t)
    Annot i a t         -> Annot i (af a) (go t)
    Concat i ts         -> Concat i (fmap go ts)
{-# INLINE fual #-}

instance (Hashable i, Eq i) => Semigroup (NE i d u m a l) where
  Concat i1 t1 <> Concat i2 t2 = Concat i' (t1 <> t2)
    where
      i' = combineMaps (Seq.length t1) i1 (Seq.length t2) i2

  Concat i1 t1 <> t2 = Concat i' (t1 |> t2)
    where
      i' = combineMaps (Seq.length t1) i1 (-1) (gi t2)

  t1 <> Concat i2 t2 = Concat i' (t1 <| t2)
    where
      i' = combineMaps (-1) (gi t1) (Seq.length t2) i2

  t1 <> t2 = Concat i' (Seq.fromList [t1,t2])
    where
      i' = combineMaps (-1) (gi t1) (-1) (gi t2)
  {-# INLINE (<>) #-}

instance Monoid d => FunctorWithIndex d (NE i d u m a) where
  imap f = go mempty where
    go !d = \case
      Leaf u l             -> Leaf u (f d l)
      Up   u               -> Up u
      UpMod i m t          -> UpMod i m (go d t)
      Label i md EmptyDUAL -> Label i md EmptyDUAL
      Label i md (NE t)    -> Label i md (NE (go d t))
      Down i d' t          -> Down i d' (go (d `mappend` d') t)
      Annot i a t          -> Annot i a (go d t)
      Concat i ts          -> Concat i (fmap (go d) ts)
      -- push down version
      -- Leaf u l               -> Leaf (act d u) (f d l)
      -- Up u                   -> Up (act d u)
      -- Label i u d' EmptyDUAL -> Label i (act d u) (Just d <> d') EmptyDUAL
      -- Label i u d' (NE t)    -> Label i (act d u) (Just d <> d') (NE (go d t))
      -- Down i u d' t          ->
      --   let !d''= d <> d'
      --   in  Down i (act d u) d'' (NE (go d'' t))
      -- Annot i u a t          -> Annot i (act d u) (act d a) (go d t)
      -- Concat i u ts          -> Concat i (act d u) (fmap go ts)

  {-# INLINE imap #-}

instance Monoid d => FoldableWithIndex d (NE i d u m a) where
  ifoldr f = go mempty where
    go !d b = \case
      Leaf _ l             -> f d l b
      Up   _               -> b
      UpMod _ _ _          -> b
      Label _ _d EmptyDUAL -> b
      Label _ _ (NE t)     -> go d b t
      Down _ d' t          -> go (d `mappend` d') b t
      Annot _ _ t          -> go d b t
      Concat _ ts          -> foldr (\t b' -> go d b' t) b ts
  {-# INLINE ifoldr #-}

  ifolded = ifoldring ifoldr
  {-# INLINE ifolded #-}

  ifoldMap = ifoldMapOf ifolded
  {-# INLINE ifoldMap #-}

itraversedNE' :: Monoid d => IndexedTraversal d (NE i d u m a l) (NE i d u m a l') l l'
itraversedNE' f = go mempty where
  go !d = \case
    Leaf u l             -> Leaf u <$> (indexed f d l)
    Up   u               -> pure (Up u)
    UpMod i fu t         -> UpMod i fu <$> go d t
    Label i md EmptyDUAL -> pure (Label i md EmptyDUAL)
    Label i md (NE t)    -> Label i md . NE <$> go d t
    Down i d' t          -> Down i d' <$> go (d `mappend` d') t
    Annot i a t          -> Annot i a <$> go d t
    Concat i ts          -> Concat i <$> traverse (go d) ts
{-# INLINE itraversedNE' #-}

itraversedNE :: Monoid d => IndexedTraversal d (NE i d u m a l) (NE i d u m a l') l l'
itraversedNE = conjoined traverse itraversedNE'
{-# INLINE [0] itraversedNE #-}

{-# RULES
 "NE itraversed/map"
   itraversedNE = sets fmap :: ASetter (NE i d u m a l) (NE i d u m a l') l l';
 "NE itraversed/imap"
   itraversedNE = isets imap :: Monoid d => AnIndexedSetter d (NE i d u m a l) (NE i d u m a l') l l';
 "NE itraversed/fold"
   itraversedNE = foldring foldr :: Getting (Endo r) (NE i d u m a l) l;;
 "NE itraversed/ifold"
   itraversedNE = ifoldring ifoldr :: Monoid d => IndexedGetting d (Endo r) (NE i d u m a l) l;;
 #-}

instance Monoid d => TraversableWithIndex d (NE i d u m a) where
  itraversed = itraversedNE
  {-# INLINE itraversed #-}
  itraverse = itraverseOf itraversedNE
  {-# INLINE itraverse #-}

-- | Push all down annotations to just above the leafs, acting of @u@ and
--   @a@ annotations along the way. (Mainly for benchmarking/debugging).
pushDown :: (Action d m, Action d a, Monoid' d) => NE i d u m a l -> NE i d u m a l
pushDown = go where
  go = \case
    Leaf u l             -> Leaf u l
    Up u                 -> Up u
    UpMod i f t          -> UpMod i f (go t)
    Label i md EmptyDUAL -> Label i md EmptyDUAL
    Label i md (NE t)    -> Label i md (NE (go t))
    Down i d t           -> Down i d (go2 d t)
    Annot i a t          -> Annot i a (go t)
    Concat i ts          -> Concat i (fmap go ts)

  go2 !d = \case
    Leaf u l             -> Down NoLabels d $ Leaf u l
    Up u                 -> Up u
    UpMod i m t          -> UpMod i (act d m) (go2 d t)
    Label i md EmptyDUAL -> Label i (Just d <> md) EmptyDUAL
    Label i md (NE t)    -> Label i (Just d <> md) (NE (go2 d t))
    Down _ d' t          -> go2 (d <> d') t
    Annot i a t          -> Annot i (act d a) (go2 d t)
    Concat i ts          -> Concat i (fmap (go2 d) ts)
{-# INLINE pushDown #-}


-- -- instance (NFData i, NFData d, NFData u, NFData a, NFData l) => NFData (NE i d u m a l) where
-- --   rnf (Leaf u l)    = rnf u `seq` rnf l
-- --   rnf (Up u)        = rnf i `seq` rnf u
-- --   rnf (Label i u d t) = rnf i `seq` rnf u `seq` rnf d `seq` rnf l
-- --   rnf (Down i u d t)  = rnf u `seq` rnf d `seq` rnf t
-- --   rnf (Annot i u a t) = rnf u `seq` rnf a `seq` rnf t
-- --   rnf (Concat i u s)  = rnf u `seq` rnf s
-- --   {-# INLINE rnf #-}

------------------------------------------------------------------------
-- DUALTree
------------------------------------------------------------------------

-- | Rose (n-ary) trees with both upwards- (/i.e./ cached) and
--   downwards-traveling (/i.e./ accumulating) monoidal annotations.
--   Abstractly, a DUALTree is a rose (n-ary) tree with data (of type
--   @l@) at leaves, data (of type @a@) at internal nodes, and two
--   types of monoidal annotations, one (of type @u@) travelling
--   \"up\" the tree and one (of type @d@) traveling \"down\".  See
--   the documentation at the top of this file for full details.
--
--   @DUALTree@ comes with some instances:
--
--   * 'Functor', for modifying leaf data.  Note that 'fmap' of course
--     cannot alter any @u@ annotations.
--
--   * 'Semigroup'. @DUALTreeNE@s form a semigroup where @(\<\>)@
--     corresponds to adjoining two trees under a common parent root,
--     with @sconcat@ specialized to put all the trees under a single
--     parent.  Note that this does not satisfy associativity up to
--     structural equality, but only up to observational equivalence
--     under 'flatten'.  Technically using 'foldDUAL' directly enables
--     one to observe the difference, but it is understood that
--     'foldDUAL' should be used only in ways such that reassociation
--     of subtrees \"does not matter\".
--
--   * 'Monoid'. The identity is the empty tree.

-- | A non-empty DUAL-tree paired with a cached @u@ value.  These
--   should never be constructed directly; instead, use 'pullU'.
data IDUAL i d u m a l
  = NE !(NE i d u m a l)
  | EmptyDUAL
  deriving (Functor, Foldable, Traversable, Typeable)

ne :: Traversal (IDUAL i d u m a l) (IDUAL i' d' u' m' a' l') (NE i d u m a l) (NE i' d' u' m' a' l')
ne _ EmptyDUAL = pure EmptyDUAL
ne f (NE t)    = NE <$> f t

instance (Hashable i, Eq i) => Semigroup (IDUAL i d u m a l) where
  NE t1     <> NE t2     = NE (t1 <> t2)
  EmptyDUAL <> a         = a
  a         <> EmptyDUAL = a
  {-# INLINE (<>) #-}

instance (Hashable i, Eq i) => Monoid (IDUAL i d u m a l) where
  mappend = (<>)
  {-# INLINE mappend #-}
  mempty  = EmptyDUAL
  {-# INLINE mempty #-}

instance Monoid d => FunctorWithIndex d (IDUAL i d u m a) -- where
  -- imapped = ne .> imapped
  -- {-# INLINE imapped #-}

instance Monoid d => FoldableWithIndex d (IDUAL i d u m a) -- where
  -- ifolded = ne .> ifolded
  -- {-# INLINE ifolded #-}

instance Monoid d => TraversableWithIndex d (IDUAL i d u m a) where
  itraversed = ne .> itraversedNE
  {-# INLINE itraversed #-}
  itraverse = itraverseOf itraversed
  {-# INLINE itraverse #-}

-- -- instance (NFData i, NFData d, NFData u, NFData a, NFData l) => NFData (IDUAL i d u m a l) where
-- --   rnf (NE t)    = rnf t
-- --   rnf EmptyDUAL = ()
-- --   {-# INLINE rnf #-}

-- | Rebuild a sequence of non-empty trees to a full tree.
rebuildSeq :: (Hashable i, Eq i) => Seq (NE i d u m a l) -> IDUAL i d u m a l
rebuildSeq (s :< ss)
  | null ss   = NE s
  | otherwise = NE $ foldr (<>) s ss
rebuildSeq _  = EmptyDUAL
{-# INLINE rebuildSeq #-}

------------------------------------------------------------------------
-- Convenience methods etc.
------------------------------------------------------------------------

-- | Construct a leaf node from a @u@ annotation along with a leaf.
leaf :: u -> l -> IDUAL i d u m a l
leaf u l = NE (Leaf u l)
{-# INLINE leaf #-}

-- | Construct an DUALTree that only contains a @u@ annotation.
leafU :: u -> IDUAL i d u m a l
leafU = NE . Up
{-# INLINE leafU #-}

modU :: m -> IDUAL i d u m a l -> IDUAL i d u m a l
modU _ EmptyDUAL = EmptyDUAL
modU m (NE t)    = NE $ UpMod (addAnnot $ gi t) m t
{-# INLINE modU #-}

-- | Add a label to the top of an IDUAL.
label :: (Eq i, Hashable i) => i -> IDUAL i d u m a l -> IDUAL i d u m a l
label i = label' (Labels (HM.singleton i (Set.singleton startTape))) Nothing
{-# INLINE label #-}

-- | Add multiple labels to the top of an IDUAL.
labels :: (Hashable i, Eq i) => [i] -> IDUAL i d u m a l -> IDUAL i d u m a l
labels is = label' (Labels $ HM.fromList (zip is (repeat $ Set.singleton startTape))) Nothing
{-# INLINE labels #-}

-- | Reset the labels, making all previous names invisible.
resetLabels :: (Hashable i, Eq i) => IDUAL i d u m a l -> IDUAL i d u m a l
resetLabels = label' NoLabels Nothing
{-# INLINE resetLabels #-}

-- | Add a label to the top of an IDUAL along. Adding a 'NoLabels' will
--   remove all reference to any names below.
label' :: (Hashable i, Eq i) => Labels i -> Maybe d -> IDUAL i d u m a l -> IDUAL i d u m a l
label' ls md t = NE (Label ls' md t)
  where
    ls' =
      case getI t of
        NoLabels  -> ls
        Labels m0 -> case ls of
          NoLabels -> NoLabels
          Labels m -> Labels $ HM.unionWith Set.union m0 m
{-# INLINE label' #-}

-- | Add an annotation to the tapes don't point to subtree behind
--   concats.
addAnnot :: Labels i -> Labels i
addAnnot NoLabels   = NoLabels
addAnnot (Labels m) = Labels $ fmap (Set.map add') m
  where add' (T [] i) = T [] (i+1)
        add' t        = t

-- | Add an internal data value at the root of a tree.  Note that this
--   only works on /non-empty/ trees; on empty trees this function is
--   the identity. O(1)
annot :: a -> IDUAL i d u m a l -> IDUAL i d u m a l
annot _ EmptyDUAL = EmptyDUAL
annot a (NE t)    = NE (Annot (addAnnot $ gi t) a t)
{-# INLINE annot #-}

-- | Apply a @d@ annotation at the root of a tree, transforming all
--   @u@ annotations by the action of @d@.
down :: Semigroup d => d -> IDUAL i d u m a l -> IDUAL i d u m a l
down _ EmptyDUAL = EmptyDUAL
down d (NE t)    = NE $ case t of
  Down i d' t' -> Down i (d <> d') t'
  _            -> Down (gi t) d t
{-# INLINE down #-}

-- | Get top level up annotation of a non-empty DUALTree.
getU :: (Action d u, Action m u, Monoid u) => IDUAL i d u m a l -> Maybe u
getU (NE t) = Just (gu t)
getU _      = Nothing
{-# INLINE getU #-}

-- | Fold each u annotation in order.
foldU :: (Action d u, Action m u) => (u -> b -> b) -> b -> IDUAL i d u m a l -> b
foldU = \f b0 -> \case
  NE t      -> fldU f b0 t
  EmptyDUAL -> b0
{-# INLINE foldU #-}

-- | Get top level up annotation of a non-empty DUALTree.
getI :: IDUAL i d u m a l -> Labels i
getI (NE t) = gi t
getI _      = NoLabels
{-# INLINE getI #-}

-- | Map over the @u@ annotation of a DUALTree.
--
--   If you want 'mapU' to commute with monoid composition, that is,
--   @mapU f (d1 \<\> d2) === mapU f d1 \<\> mapU f d2@, it suffices
--   to ensure that @f@ is a monoid homomorphism, that is, @f mempty =
--   mempty@ and @f (u1 \<\> u2) = f u1 \<\> f u2@.  Additionally,
--   @mapU f@ will commute with @act d@ if @f@ does.
mapU :: m -> IDUAL i d u m a l -> IDUAL i d u m a l
mapU m (NE t) = NE (UpMod (addAnnot $ gi t) m t)
mapU _ _      = EmptyDUAL
{-# INLINE mapU #-}

-- | Similar to 'mapU' but also allows changing @a@ and @l@ in a single
--   pass.
mapUAL :: (u -> u') -> (a -> a') -> (l -> l') -> IDUAL i d u m a l -> IDUAL i d u' m a' l'
mapUAL uf af lf (NE t) = NE (fual uf af lf t)
mapUAL _  _  _  _      = EmptyDUAL
{-# INLINE mapUAL #-}


-- -- | Apply a @u@ annotation of a DUALTree on the left. Makes a 'leafU'
-- --   for an empty tree.
-- preapplyU :: (Hashable i, Eq i, Semigroup u) => u -> IDUAL i d u m a l -> IDUAL i d u m a l
-- preapplyU u t = leafU u <> t
-- {-# INLINE preapplyU #-}

-- -- | Apply an @u@ annotation of a DUALTree on the right. Makes a 'leafU'
-- --   for an empty tree.
-- postapplyU :: (Hashable i, Eq i, Semigroup u) => u -> IDUAL i d u m a l -> IDUAL i d u m a l
-- postapplyU u t = t <> leafU u
-- {-# INLINE postapplyU #-}

-- ------------------------------------------------------------
-- -- Folds
-- ------------------------------------------------------------

-- | Fold a dual tree for a monoidal result @r@. The @d@ annotations are
--   accumulated from the top of the tree. Static @a@ annotations are
--   acted on by the @d@ annotation accumulated up to that point.
foldDUAL
  :: (Action d a, Monoid d, Monoid r)
  => (d -> l -> r) -- ^ Process a leaf
  -> (a -> r -> r) -- ^ Process an annotation
  -> IDUAL i d u m a l
  -> r
foldDUAL _  _  EmptyDUAL = mempty
foldDUAL lF aF (NE t0)   = go mempty t0 where
  go !d = \case
    Leaf _ l            -> lF d l
    Up _                -> mempty
    UpMod _ _ t         -> go d t
    Label _ _ EmptyDUAL -> mempty
    Label _ _ (NE t)    -> go d t
    Down _ d' t         -> go (d `mappend` d') t
    Annot _ a t         -> aF (act d a) (go d t)
    Concat _ ts         -> F.foldMap (go d) ts
{-# INLINE foldDUAL #-}

------------------------------------------------------------------------
-- Subtrees
------------------------------------------------------------------------

-- SubIDUAL ------------------------------------------------------------


-- Benchmarks need to be done to determine if it's better to store a
-- SubIDUAL as a pretext or as separate functions.
--
-- newtype SubIDUAL i d u m a l = SubIDUAL (Indexable d p => Pretext p (IDUALTree i d u a l) (IDUALTree i d u a l))
--   = SubIDUAL {runSubIDUAL :: forall f p. (Indexable d p, Functor f) => p a (f b) -> f t}
-- sub :: IndexedLens' d (SubIDUAL
--   = Pretext {runPretext :: forall (f :: * -> *).  Functor f => p a (f b) -> f t}
-- sub :: IndexedLens' d (SubIDUAL i d u m a l) (IDUAL i d u m a l)
-- sub f p = runPretext p s <&> \s' -> iseek s' p
-- subPeeks :: (IDUAL i d u m a l -> IDUAL i d u m a l) -> SubIDUAL i d u m a l -> IDUAL i d u m a l
-- subPeeks = coerce (ipeeks (IDUAL i d u m a l -> IDUAL i d u m a l) -> Pretext (->) (IDUAL i d u m a l) (IDUAL i d u m a l))

-- | A subtree in the context of a full tree. It gives access to the
--   targeted subtree as well as a function that takes a modified
--   subtree and returns the original tree with the modified subtree in
--   the old ones place (why can't I write sentences that make sense?).
data SubIDUAL i d u m a l = SubIDUAL
  { subPos    :: IDUAL i d u m a l
  , subPeek   :: IDUAL i d u m a l -> IDUAL i d u m a l
  , accumDown :: Maybe d
  -- , subTape   :: Tape
  -- , subNames  :: [i]
  -- The tape is not extractable. It's used to give subIDUAL a unique
  -- value so it can be stored in a set (nope, can't do that, maybe a
  -- seperate type that includes the tape that isn't exported, just for
  -- ord instance in submap?)

  }

-- | Lens onto the targeted sub tree with an index of the down
--   annotations accumulated since the subtree was named.
sub :: IndexedLens' (Maybe d) (SubIDUAL i d u m a l) (IDUAL i d u m a l)
sub g (SubIDUAL s f d) = indexed g d s <&> \s' -> SubIDUAL s' f d

-- | Take a function that modified the subtree and return the original
--   IDUAL with the target subtree modified.
subPeeks :: (IDUAL i d u m a l -> IDUAL i d u m a l) -> SubIDUAL i d u m a l -> IDUAL i d u m a l
subPeeks g (SubIDUAL s f _) = f (g s)

mkSubDUAL :: (Hashable i, Eq i, Semigroup d, Semigroup u, Action d u, Monoid u)
  => IDUAL i d u m a l -> Tape -> SubIDUAL i d u m a l
mkSubDUAL t tape = SubIDUAL
  { subPos    = sb
  , subPeek   = flip ipeek p
  , accumDown = d
  }
  where
  (d, sb) = getConst $ runPretext p (Const #. Indexed (,))
  p       = mkPretext tape t

-- | A list of all subtrees together with the labels for that subtree.
--   (is the order of the i's correct?)
allSubs
  :: (Hashable i, Eq i, Semigroup d, Semigroup u, Action d u, Monoid u)
  => IDUAL i d u m a l -> [([i], SubIDUAL i d u m a l)]
-- allSubs t = ifoldr (\tp is ss -> (is, mkSubDUAL t tp) : ss) [] (allTapes (labels t))
allSubs t = map (\(tp,is) -> (is, mkSubDUAL t tp)) (itoList (allTapes (getI t)))

-- XXX Is it better to just go through the whole diagram and collect all
-- labeled names?

allTapes :: Labels i -> Map Tape [i]
allTapes NoLabels   = mempty
allTapes (Labels (HM.toList -> tis0)) = go mempty tis0 where
  -- For each tape, add the label i, starting a new list of labels if
  -- the tape isn't in the map yet.
  go its ((i,ts):tis) = go (foldr (M.alter f) its ts) tis
    where f Nothing   = Just [i]
          f (Just is) = Just $ i : is
  go its []           = its

traverseSub
  :: (Hashable i, Eq i, Action d u, Semigroup d, Monoid d, Semigroup u)
  => [i] -> Traversal' (IDUAL i d u m a l) (IDUAL i d u m a l)
traverseSub (i0:is) f t = route (mkRoute $ Set.toList ts) f t where
  ts = case getI t of
    Labels m -> foldr (\i s -> Set.intersection s (m ^. ix i)) (m ^. ix i0) is
    NoLabels -> Set.empty
traverseSub [] _ t = pure t

------------------------------------------------------------------------
-- Traversing
------------------------------------------------------------------------

ixDUAL
  :: forall p f i d u m a l.
  (Indexable (Maybe d) p, Hashable i, Eq i, Action d u, Functor f, Semigroup d, Semigroup u, Monoid u)
  => Tape
  -> (p (IDUAL i d u m a l) (f (IDUAL i d u m a l)))
  -> IDUAL i d u m a l
  -> f (IDUAL i d u m a l)
ixDUAL _       _ (EmptyDUAL) = error "tape used on EmptyDUAL"
ixDUAL (T p n) f (NE t0)     = go Nothing p t0 where
  -- XXX Need two versions of go, one that hasn't seen a d annotation
  -- yet and one that has one. This would give us the nicer Semigroup
  -- constraint on d and prevent accumulating unnecessary empty d
  -- annotations.
  mdown :: Maybe d -> IDUAL i d u m a l -> IDUAL i d u m a l
  mdown = maybe id down

  mapp :: Maybe d -> d -> d
  mapp = maybe id (<>)

  -- follow the tape path to the target subtree
  go :: Maybe d -> [Int] -> NE i d u m a l -> f (IDUAL i d u m a l)
  go d []         = go' d n
  go d iss@(i:is) = \case
    Down _ d' t        -> go (Just $ d `mapp` d') iss t
    UpMod _ m t        -> mapU m <$> go d iss t
    Annot _ a t        -> annot a <$> go d iss t
    Label lb d' (NE t) -> label' lb (d <> d') <$> go d iss t
    Concat _ ts
      | (sL, t :< sR) <- Seq.splitAt i ts ->
          go d is t <&> \t' -> fromSeq sL <> t' <> fromSeq sR
          where fromSeq = mdown d . rebuildSeq
    b -> error $ "ixDUAL: tape follow failed: " ++ neShow b

  -- Once we've followed to path through the concats, we pass
  -- n annotation nodes to get to the correct Label
  go' :: Maybe d -> Int -> NE i d u m a l -> f (IDUAL i d u m a l)
  go' _ i | i < 0 = error "ixDUAL: malformed tape: negative expected annotations"
  go' d 0 = \case
    Label i d' t -> let d'' = d <> d' in label' i d'' <$> indexed f (d <> d') (mdown d t)
    Down _ d' t  -> go' (Just $ d `mapp` d') 0 t
    b              -> error $ "ixDUAL: expected label node: " ++ neShow b
  go' d i = \case
    Down _ d' t -> go' (Just $ d `mapp` d') i t
    Annot _ a t -> annot a <$> go' d (i - 1) t
    UpMod _ m t -> mapU m <$> go' d (i - 1) t
    _           -> error $ "ixDUAL: wrong number of expected annotations: " ++ show i

type IDUALPretext i d u m a l =
  forall p. Indexable (Maybe d) p => Pretext' p (IDUAL i d u m a l) (IDUAL i d u m a l)

-- Make a pretext for a tape you know points to a subtree of this tree.
-- Will error for incorrect tape.
mkPretext
  -- :: (Hashable i, Monoid d, Semigroup u, Action d u)
  :: (Hashable i, Eq i, Semigroup d, Semigroup u, Action d u, Monoid u)
  => Tape -> IDUAL i d u m a l -> IDUALPretext i d u m a l
mkPretext tape t = Pretext (\f -> ixDUAL tape f t)
{-# INLINE mkPretext #-}

-- -- Traverse a route ----------------------------------------------------

-- | Traverse a rogue going to the lowest case in the tree.
route
  :: forall f i d u m a l. (Hashable i, Eq i, Action d u, Applicative f, Semigroup d, Monoid d, Semigroup u)
  -- => DUALIndex i d u a l
  => Route
  -> (IDUAL i d u m a l -> f (IDUAL i d u m a l))
  -> IDUAL i d u m a l
  -> f (IDUAL i d u m a l)
route (Route [] []) _ EmptyDUAL    = pure EmptyDUAL
route _  _ EmptyDUAL = error "tape used on EmptyDUAL"
route r0 f (NE t0)   = go mempty r0 t0 where
  -- XXX Need two versions of go, one that hasn't seen a d annotation
  -- yet and one that has one. This would give us the nicer Semigroup
  -- constraint on d and prevent accumulating unnecessary empty d
  -- annotations.
  -- follow the tape path to the target subtree

  -- In this varient we only traverse the last occurence of the route.
  -- This means we ignore the (Route (0:ns) is) case.
  go :: d -> Route -> NE i d u m a l -> f (IDUAL i d u m a l)
  go d (Route [] [])     = pure . down d . NE
  -- go d (Route (0:ns) is) = \t -> f =<< go d (Route ns is) t
  go d (Route [0] _) = f . down d . NE
  go d (Route (0:ns) is) = go d (Route ns is)
  go d r@(Route ns@(_:_) is) = \case
    Down _ d' t -> go (d `mappend` d') r t
    UpMod _ m t -> modU m <$> go d r' t
    Annot _ a t -> annot a <$> go d r' t
    Label lb d' (NE t) -> label' lb (Just d <> d') <$> go d r t -- XXX NOT SURE ABOUT THIS
    t           -> error $ "rotue: reached " ++ neShow t ++ " with "
                         ++ show (head ns) ++ " expected annotations"
    where r' = Route (map (subtract 1) ns) is
  go d r@(Route [] is) = \case
    Down _ d' t -> go (d `mappend` d') r t
    UpMod _ m t -> modU m <$> go d r t
    Annot _ a t -> annot a <$> go d r t
    Concat _ s  -> go2 d is s
    t -> error $ "route: reached " ++ neShow t ++ " before following path"

  go2 :: d -> [(Int, Route)] -> Seq (NE i d u m a l) -> f (IDUAL i d u m a l)
  go2 d ((i,r):is) ts
    | (sL, t :< sR) <- Seq.splitAt i ts =
        (\t' sR' -> down d (rebuildSeq sL) <> t' <> sR')
          <$> go d r t
          <*> go2 d (is & each . _1 -~ (i+1)) sR
        -- t'  <- go d r t
        -- sR' <- go2 d (is & each . _1 -~ (i+1)) sR
        -- pure $ down d (rebuildSeq sL) <> t' <> sR'
    -- | (sL, t :< sR) <- Seq.splitAt i ts = do
    --     t'  <- go d r t
    --     sR' <- go2 d (is & each . _1 -~ (i+1)) sR
    --     pure $ idown d (rebuildSeq sL) <> t' <> sR'
    | otherwise = error "route: tried to index wrong part of concat"
  go2 d [] s = pure (down d (rebuildSeq s))

-- All leafs -----------------------------------------------------------

leafs :: Monoid d => Traversal (IDUAL i d u m a l) (IDUAL i d u m a l') (IDUAL i d u m a l) (IDUAL i d u m a l')
leafs _ EmptyDUAL = pure EmptyDUAL
leafs f (NE t0)   = NE <$> go mempty t0 where
  go !d = \case
    Leaf u l             -> f (NE $ Down NoLabels d (Leaf u l)) <&> \case
                              NE t      -> t
                              EmptyDUAL -> Label NoLabels Nothing EmptyDUAL
    Up   u               -> f (NE $ Down NoLabels d (Up u)) <&> \case
                              NE t      -> t
                              EmptyDUAL -> Label NoLabels Nothing EmptyDUAL

    -- what to do about up modifications?
    UpMod i fu t         -> UpMod i fu <$> go d t

    Label i md EmptyDUAL -> pure (Label i md EmptyDUAL)
    Label i md (NE t)    -> Label i md . NE <$> go d t
    Down _ d' t          -> go (d `mappend` d') t
    Annot i a t          -> Annot i a <$> go d t
    Concat i ts          -> Concat i <$> traverse (go d) ts
{-# INLINE leafs #-}


-- Traversing downs ----------------------------------------------------

downs :: (Eq i, Hashable i, Action d a, Monoid' d) => Traversal' (IDUAL i d u m a l) d
downs _ EmptyDUAL = pure EmptyDUAL
downs f (NE t0)   = go mempty t0 where

  go !d = \case
    Down _ d' t        -> go (d `mappend` d') t
    UpMod _ m t        -> mapU m <$> go d t
    Annot _ a t        -> annot (act d a) <$> go d t
    Label lb md (NE t) -> -- label' lb (Just d `mapp` d) . NE <$> go
      case md of
        Just d' -> let !d'' = d `mappend` d' in label' lb (Just d'') <$> go d'' t
        Nothing -> label' lb Nothing <$> go d t
    Concat _ ts          -> foldr mappend mempty <$> traverse (go d) ts
    n                    -> f d <&> \d' -> down d' (NE n)

-- Traversing ups ------------------------------------------------------

-- | Match leafs whose up annotations match a predicate. Any up
-- modifications are ignored
matchingU :: (Action d u, Monoid d) => (u -> Bool) -> Traversal' (IDUAL i d u m a l) (IDUAL i d u m a l)
matchingU _ _ EmptyDUAL  = pure EmptyDUAL
matchingU pred f (NE t0) = NE <$> go mempty t0 where

  go !d = \case
    lef@(Leaf u l)
      | pred (act d u)   -> f (NE $ Down NoLabels d (Leaf u l)) <&> \case
                              NE t      -> t
                              EmptyDUAL -> Label NoLabels Nothing EmptyDUAL
      | otherwise        -> pure lef
    Up   u               -> f (NE $ Down NoLabels d (Up u)) <&> \case
                              NE t      -> t
                              EmptyDUAL -> Label NoLabels Nothing EmptyDUAL

    UpMod i m t         -> UpMod i m <$> go d t

    Label i md EmptyDUAL -> pure (Label i md EmptyDUAL)
    Label i md (NE t)    -> Label i md . NE <$> go d t
    Down _ d' t          -> go (d `mappend` d') t
    Annot i a t          -> Annot i a <$> go d t
    Concat i ts          -> Concat i <$> traverse (go d) ts
{-# INLINE matchingU #-}

tapeMatches :: (Monoid d, Action d u) => (u -> Bool) -> IDUAL i d u m a l -> [Tape]
tapeMatches _ EmptyDUAL = []
tapeMatches pred (NE t0) = go mempty startTape t0 where
  go !d tp = \case
    Leaf u _
      | pred (act d u)   -> [tp]
      | otherwise        -> []
    Up u
      | pred (act d u)   -> [tp]
      | otherwise        -> []
    UpMod _ _ t          -> go d (tp & nannots +~ 1) t
    Label _ _ EmptyDUAL -> []
    Label _ _ (NE t)    -> go d tp t
    Down _ d' t          -> go (d `mappend` d') tp t
    Annot _ _ t          -> go d (tp & nannots +~ 1) t
    Concat _ ts          -> ifoldMap (\n -> go d (tp & path %~ flip snoc n)) ts
{-# INLINE tapeMatches #-}

-- -- Debugging -----------------------------------------------------------

idualShow :: IDUAL i d u m a l -> String
idualShow EmptyDUAL = "EMPTY"
idualShow (NE t)    = neShow t

neShow :: NE i d u m a l -> String
neShow = \case
  Leaf {}   -> "Leaf"
  Up {}     -> "Up"
  UpMod {}  -> "UpMod"
  Label {}  -> "Label"
  Down {}   -> "Down"
  Annot {}  -> "Annot"
  Concat {} -> "Concat"
{-# INLINE neShow #-}

