{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

{-# LANGUAGE PolyKinds                  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Style
-- Copyright   :  (c) 2011-2016 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A definition of /styles/ for diagrams as extensible, heterogeneous
-- collections of attributes.
--
-----------------------------------------------------------------------------

module Diagrams.Types.Style
  ( -- * Attributes
    -- $attr

    AttrKind (..)
  , AttributeClass (..)
  , AttributeSpace
  , Rep

    -- -- ** Attributes prism
  -- , attribute

    -- * Styles
    -- $style

  , Style (..)

    -- -- ** Making styles
  -- , attributeToStyle

    -- ** Attibute lenses
  , atAttr
  , backupAttr

    -- ** Applying styles
  , applyAttr
  , attributeToStyle

  , ApplyStyle (..)
  , HasStyle (..)

    -- * Extracting attributes from styles

  , Attributes
  , getAttributes
  , getAttr

  -- * Internals
  -- These shouldn't been nessesary but are exported just incase.

  ) where

-- import           Control.Applicative
import           Control.Arrow           ((***))
import           Control.Lens            hiding (transform)
import qualified Data.HashMap.Strict     as HM
import qualified Data.Map                as M
import           Data.Monoid.Action      as A
import Data.Dynamic
import           Data.Semigroup
import qualified Data.Set                as S
-- import           Data.Typeable

import           Diagrams.Types.Measure
import           Geometry.Transform hiding (T)
import           Geometry.Space

import           Linear.Vector

import GHC.Exts

------------------------------------------------------------
-- Attributes
------------------------------------------------------------

-- $attr
-- An /attribute/ is anything that determines some aspect of a
-- diagram's rendering.  The standard diagrams library defines several
-- standard attributes (line color, line width, fill color, etc.) but
-- additional attributes may easily be created.  Additionally, a given
-- backend need not handle (or even know about) attributes used in
-- diagrams it renders.
--
-- The attribute code is inspired by xmonad's @Message@ type, which
-- was in turn based on ideas in:
--
-- Simon Marlow.
-- /An Extensible Dynamically-Typed Hierarchy of Exceptions/.
-- Proceedings of the 2006 ACM SIGPLAN workshop on
-- Haskell. <http://research.microsoft.com/apps/pubs/default.aspx?id=67968>.

-- | The three possible types of attributes:
--
--     * 'IAttr' - inert attributes that are unaffected by transforms
--     * 'MAttr' - measured attributes -- transforming these multiplies
--       the 'local' measure by the average scale of the transform.
--     * 'TAttr' - transformable attributes
data AttrKind = SAttr | MAttr | TAttr

-- | The space constraint for the attribute.
type family Attribute' k a (v :: * -> *) n :: Constraint where
  Attribute' 'SAttr a v n = (AttrType a ~ 'SAttr)
  Attribute' 'MAttr a v n = (AttrType a ~ 'MAttr)
  Attribute' 'TAttr a v n = (InSpace v n a, Transformable a, AttrType a ~ 'TAttr)

-- | The constraint on for an attribute.
--
--  @
--  'AttrType' a ~ 'SAttr' => 'AttributeSpace' a v n = 'AttributeClass' a
--  'AttrType' a ~ 'MAttr' => 'AttributeSpace' a v n = 'AttributeClass' a
--  'AttrType' a ~ 'TAttr' => 'AttributeSpace' a v n = ('AttributeClass' a, 'InSpace' a v n, 'Transformable' a)
--  @
type AttributeSpace a v n = (AttributeClass a, Attribute' (AttrType a) a v n, SingAttr (AttrType a))
-- Not that Attribute class does not mention v or n so we can't have
-- this a superclass of AttributeClass.

-- | Every attribute must be an instance of @AttributeClass@  The
--   'Semigroup' instance for an attribute determines how it will combine
--   with other attributes of the same type.
class (Typeable a, Semigroup a) => AttributeClass a where
  -- | The type of attribute. Choose between 'SAttr', 'MAttr' or
  --   'TAttr'.
  type AttrType a :: AttrKind

-- | The representation used for an attribute kind.
type family Rep' k n a where
  Rep' 'SAttr n a = a
  Rep' 'MAttr n a = Measured n a
  Rep' 'TAttr n a = a

-- | The representation used for an attribute.
--
--  @
--  'AttrType' a ~ 'SAttr' => 'Rep' a n r = r
--  'AttrType' a ~ 'MAttr' => 'Rep' a n r = 'Measured' n r
--  'AttrType' a ~ 'TAttr' => 'Rep' a n r = r
--  @
type Rep a n r = Rep' (AttrType a) n r

-- Singletons for the attribute kind. This allows us bring the type of
-- attribute we're dealing with in scope.

class SingAttr (i::AttrKind) where
  sing :: AttrS i

data AttrS k where
  S :: k ~ 'SAttr => AttrS k
  M :: k ~ 'MAttr => AttrS k
  T :: k ~ 'TAttr => AttrS k

instance SingAttr 'SAttr where sing = S
instance SingAttr 'MAttr where sing = M
instance SingAttr 'TAttr where sing = T

------------------------------------------------------------------------
-- The attribute type
------------------------------------------------------------------------

-- | An existential wrapper type to hold attributes.
data Attribute (v :: * -> *) n where
  SAttribute :: (AttributeClass a, AttrType a ~ 'SAttr) => a -> Attribute v n
  MAttribute :: (AttributeClass a, AttrType a ~ 'MAttr) => Measured n a -> Attribute v n
  TAttribute :: (AttributeClass a, InSpace v n a, Transformable a, AttrType a ~ 'TAttr)
             => a -> Attribute v n

type instance V (Attribute v n) = v
type instance N (Attribute v n) = n

-- | Display the type of attribute used. Useful for debugging.
instance Show (Attribute v n) where
  showsPrec d attr = showParen (d > 10) $ case attr of
    SAttribute a -> showString "SAttribute " . showsPrec 11 (typeOf a)
    MAttribute a -> showString "MAttribute " . showsPrec 11 (mType a)
    TAttribute a -> showString "TAttribute " . showsPrec 11 (typeOf a)

-- | 'TAttribute's are transformed directly, 'MAttribute's have their
--   local scale multiplied by the average scale of the transform.
--   Static 'Attribute's are unaffected.
instance (Additive v, Traversable v, Floating n) => Transformable (Attribute v n) where
  transform _ (SAttribute a) = SAttribute a
  transform t (MAttribute a) = MAttribute $ scaleLocal (avgScale t) a
  transform t (TAttribute a) = TAttribute $ transform t a

-- | Given a isomorphim between an attribute's internal representation
--   and the one you use, make a prism on it.
--
-- @
-- 'attribute' :: 'SAttribute' a => 'AnIso'' a r -> 'Prism'' 'Attribute' r
-- 'attribute' :: 'MAttribute' a => 'AnIso'' a r -> 'Prism'' 'Attribute' ('Measured' r)
-- 'attribute' :: 'TAttribute' a => 'AnIso'' a r -> 'Prism'' 'Attribute' r
-- @
attribute :: AttributeSpace a v n => AnIso' a r -> Prism' (Attribute v n) (Rep a n r)
attribute l = withIso l $ \ar ra -> prism' (mkAttr ra) (fromAttr ar)
{-# INLINE attribute #-}

mkAttr :: forall a v n r. AttributeSpace a v n => (r -> a) -> Rep a n r -> Attribute v n
mkAttr ra r =
  case sing :: AttrS (AttrType a) of
    S -> SAttribute (ra r)
    M -> MAttribute (fmap ra r)
    T -> TAttribute (ra r)

fromAttr :: forall a r v n. AttributeClass a => (a -> r) -> Attribute v n -> Maybe (Rep a n r)
fromAttr ar = \case
  SAttribute a -> case eq a  of Just Refl -> Just (ar a);     Nothing -> Nothing
  MAttribute a -> case eqM a of Just Refl -> Just (ar <$> a); Nothing -> Nothing
  TAttribute a -> case eq a  of Just Refl -> Just (ar a);     Nothing -> Nothing
  where
    eq :: Typeable a' => a' -> Maybe (a' :~: a)
    eq _ = eqT
    eqM :: Typeable a' => Measured n a' -> Maybe (a' :~: a)
    eqM _ = eqT

-- | Type of an attribute that is stored with a style. Measured
--   attributes return the type as if it where unmeasured.
attributeType :: Attribute v n -> TypeRep
attributeType (SAttribute a) = typeOf a
attributeType (MAttribute a) = mType a
attributeType (TAttribute a) = typeOf a
{-# INLINE attributeType #-}

-- Note that we use typerep 'a' not 'Measured n a'
mType :: forall n a. Typeable a => Measured n a -> TypeRep
mType _ = typeOf (undefined :: a)

------------------------------------------------------------------------
-- Styles
------------------------------------------------------------------------

-- $style
-- A 'Style' is a heterogeneous collection of attributes, containing
-- at most one attribute of any given type.  This is also based on
-- ideas stolen from xmonad, specifically xmonad's implementation of
-- user-extensible state.

-- | A @Style@ is a heterogeneous collection of attributes, containing
--   at most one attribute of any given type.
newtype Style v n = Style (HM.HashMap TypeRep (Attribute v n))

-- instances -----------------------------------------------------------

type instance V (Style v n) = v
type instance N (Style v n) = n

_Style :: Iso' (Style v n) (HM.HashMap TypeRep (Attribute v n))
_Style = coerced
{-# INLINE _Style #-}

-- | Combine a style by combining the attributes; if the two styles have
--   attributes of the same type they are combined according to their
--   semigroup structure.
instance Semigroup (Style v n) where
  Style s1 <> Style s2 = Style $ HM.unionWith combineAttr s1 s2 where

    -- type signature needed for some reason
    combineAttr :: Attribute v n -> Attribute v n -> Attribute v n
    combineAttr (SAttribute a1) (SAttribute a2') | Just a2 <- cast a2'  = SAttribute (a1 <> a2)
    combineAttr (MAttribute a1) (MAttribute a2') | Just a2 <- castM a2' = MAttribute (a1 <> a2)
    combineAttr (TAttribute a1) (TAttribute a2') | Just a2 <- cast a2'  = TAttribute (a1 <> a2)
    combineAttr a1 a2 = error $ "internal error: attributes for " ++ show a1 ++ " and "
                         ++ show a2 ++ " where stored under the same TypeRep"

-- A cast for the measured attribute so we don't need a (Typeable n)
-- constraint.
castM :: forall n a b. (Typeable a, Typeable b) => Measured n a -> Maybe (Measured n b)
castM m =
  case eqT :: Maybe (a :~: b) of
    Just Refl -> Just m
    Nothing   -> Nothing

-- | The empty style contains no attributes.
instance Monoid (Style v n) where
  mempty  = Style HM.empty
  mappend = (<>)

-- | Transform all 'TAttr's, scale all 'MAttr's.
instance (Additive v, Traversable v, Floating n) => Transformable (Style v n) where
  transform t = _Style . mapped %~ transform t

-- | Styles have no action on other monoids.
instance A.Action (Style v n) m

-- | Show the type of the attributes in the style.
instance Show (Style v n) where
  showsPrec d sty = showParen (d > 10) $
    showString "Style " . showsPrec d (sty ^.. _Style . folded)

-- making styles -------------------------------------------------------

attributeToStyle :: Attribute v n -> Style v n
attributeToStyle a = Style $ HM.singleton (attributeType a) a

-- | Create a style with a single attribute.
--
-- @
-- 'attrToStyle' 'Diagrams.Attributes._Opacity' '0.5' :: 'Style' v n
-- @
attrToStyle :: AttributeSpace a v n => AReview a r -> Rep a n r -> Style v n
attrToStyle l r = attributeToStyle (mkAttr (review l) r)

-- style lenses --------------------------------------------------------

-- | Given a isomorphim between an attribute's internal representation
--   and the one you use, make a lens on it.
--
-- @
-- 'atAttr' :: 'AttrType` a ~ 'SAttr' => 'AnIso'' a r -> 'Lens'' ('Style' v n) ('Maybe' r)
-- 'atAttr' :: 'AttrType` a ~ 'MAttr' => 'AnIso'' a r -> 'Lens'' ('Style' v n) ('Maybe' ('Measured' n r))
-- 'atAttr' :: 'AttrType` a ~ 'TAttr' => 'AnIso'' a r -> 'Lens'' ('Style' v n) ('Maybe' r)
-- @
--
--   There shouldn't be any need to use this for any of the standard
--   attributes since they all have their own versions:
--
-- @
-- 'Diagrams.Attributes._lineWidth = 'atAttr' '_LineWidth'
-- @
atAttr :: AttributeSpace a v n => AnIso' a r -> Lens' (Style v n) (Maybe (Rep a n r))
atAttr l f sty =
  f (sty ^? _Style . ix ty . attribute l) <&>
    \r' -> sty & _Style . at ty .~ fmap (attribute l #) r'
  where
  ty = getTy l -- typeRep (Proxy @ a)
  -- I hate top level signatures with forall.
  getTy :: forall a r. Typeable a => AnIso' a r -> TypeRep
  getTy _ = typeRep (Proxy @ a)
{-# INLINE atAttr #-}

-- Backup styles -------------------------------------------------------

-- | Internal wrapper for backup attributes.
newtype Backup a = Backup a
  deriving (Show, Num, Transformable)

type instance V (Backup a) = V a
type instance N (Backup a) = N a

instance AttributeClass a => AttributeClass (Backup a) where
  type AttrType (Backup a) = AttrType a

_Backup :: Iso' (Backup a) a
_Backup = coerced
{-# INLINE _Backup #-}

instance Semigroup a => Semigroup (Backup a) where
  (<>) = coerce ((<>) :: a -> a -> a)
  {-# INLINE (<>) #-}

-- | Similar to 'atAttr' but for backup attributes. These are attributes
--   that are only used if the attribute is not otherwise set (before or
--   after setting an backup attr).
backupAttr :: (AttributeSpace (Backup a) v n)
           => AnIso' a r -> Lens' (Style v n) (Maybe (Rep a n r))
backupAttr l = atAttr (_Backup . l)
{-# INLINE backupAttr #-}

-- Applying styles -----------------------------------------------------

-- | Type class for things which can have a style applied.
class ApplyStyle a where
  -- | /Apply/ a style by combining it (on the left) with the existing
  --   style.
  applyStyle :: Style (V a) (N a) -> a -> a

instance ApplyStyle (Style v n) where
  applyStyle = mappend

instance (ApplyStyle a, ApplyStyle b, V a ~ V b, N a ~ N b) => ApplyStyle (a,b) where
  applyStyle s = applyStyle s *** applyStyle s

instance ApplyStyle a => ApplyStyle [a] where
  applyStyle = fmap . applyStyle

instance ApplyStyle b => ApplyStyle (a -> b) where
  applyStyle = fmap . applyStyle

instance ApplyStyle a => ApplyStyle (M.Map k a) where
  applyStyle = fmap . applyStyle

instance (ApplyStyle a, Ord a) => ApplyStyle (S.Set a) where
  applyStyle = S.map . applyStyle

instance ApplyStyle b => ApplyStyle (Measured n b) where
  applyStyle = fmap . applyStyle

-- | Apply an attribute to an instance of 'ApplyStyle' (such as a
--   diagram or a style). If the object already has an attribute of
--   the same type, the new attribute is combined on the left with the
--   existing attribute, according to their semigroup structure.
applyAttr :: (AttributeSpace a (V d) (N d), ApplyStyle d)
          => AReview a r -> Rep a (N d) r -> d -> d
applyAttr l = applyStyle . attrToStyle l
{-# INLINE applyAttr #-}

-- | Class of things with a single style.
class ApplyStyle a => HasStyle a where
  style :: Lens' a (Style (V a) (N a))

instance HasStyle (Style v n) where
  style = id
  {-# INLINE style #-}

-- Compiling styles ----------------------------------------------------

-- | Turn an 'MAttribute' into an 'Attribute' using the given 'global'
--   and 'normalized' scale.
readyAttribute :: Num n => n -> n -> Attribute v n -> Dynamic
readyAttribute _ _ (SAttribute a) = toDyn a
readyAttribute g n (MAttribute a) = toDyn (fromMeasured g n a)
readyAttribute _ _ (TAttribute a) = toDyn a
{-# INLINE readyAttribute #-}

-- | A gathering of attributes ready to be rendered.
newtype Attributes = RAs (HM.HashMap TypeRep Dynamic)
  deriving (Semigroup, Monoid)

instance Rewrapped Attributes Attributes
instance Wrapped Attributes where
  type Unwrapped Attributes = (HM.HashMap TypeRep Dynamic)
  _Wrapped' = coerced
  {-# INLINE _Wrapped' #-}

instance Each Attributes Attributes Dynamic Dynamic where
  each = _Wrapped . traversed
  {-# INLINE each #-}

-- | Extract the 'Attribute's from a style using the given 'global' and
--   'normalized' scale.
getAttributes :: Num n => n -> n -> Style v n -> Attributes
getAttributes g n (Style hm) = RAs (HM.map (readyAttribute g n) hm)
{-# INLINE getAttributes #-}

-- | Extract a single attribute, ready to be rendered. This function
--   first looks looks
getAttr :: Typeable a => Getting r a r -> Attributes -> Maybe r
getAttr g (RAs hm) =
  case HM.lookup (getterRep g) hm of
    Just a  -> view g <$> fromDynamic a
    Nothing -> case HM.lookup (getterRep (_Backup . g)) hm of
      Just a  -> view (_Backup . g) <$> fromDynamic a
      Nothing -> Nothing
{-# INLINE getAttr #-}

getterRep :: forall a s. Typeable s => Getting a s a -> TypeRep
getterRep _ = typeRep (Proxy :: Proxy s)
