{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Text
-- Copyright   :  (c) 2011-2015 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Very basic text primitives along with associated attributes.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Text (
  -- * Creating text diagrams
    Text(..)
  , TextAlignment(..)
  , text, topLeftText, alignedText, baselineText
  , mkText, mkText'

  -- * Text attributes

  -- ** Font family
  , Font, _Font
  , font, _font

  -- ** Font size
  , FontSize, _FontSize
  , fontSize, _fontSize
  , fontSizeN, fontSizeO , fontSizeL , fontSizeG

  -- ** Font slant
  , FontSlant(..), _FontSlant
  , fontSlant, italic, oblique, _fontSlant

  -- ** Font weight
  , FontWeight(..), _FontWeight
  , fontWeight, _fontWeight
  , bold, bolder, lighter, thinWeight
  , ultraLight, light, mediumWeight
  , heavy, semiBold, ultraBold

  ) where

import           Control.Lens             hiding (transform)
import           Data.Colour              hiding (over)
import           Data.Default.Class
#if __GLASGOW_HASKELL__ < 710
import           Data.Functor
#endif
import           Data.Semigroup
import           Data.Typeable
import           Linear.Affine (origin)

import           Geometry.Space
import           Geometry.Envelope   (pointEnvelope)
import           Geometry.TwoD.Types

import           Diagrams.Types
import           Diagrams.TwoD.Attributes
import           Diagrams.Attributes


------------------------------------------------------------
-- Text diagrams
------------------------------------------------------------

-- | A 'Text' primitive consists of the string contents, text alignment
--   and the transformation to be applied. The transformation is scale
--   invarient, the average scale of the transform should always be 1.
--   All text scaling is obtained from the 'FontSize' attribute.
--
--   This constructor should not be used directly. Use 'text',
--   'alignedText' or 'baselineText'.
data Text n = Text (TextAlignment n) String
  deriving Typeable
  -- XXX When rendering text you should only use a transform with
  -- an average scale of one

type instance V (Text n) = V2
type instance N (Text n) = n

-- | @TextAlignment@ specifies the alignment of the text's origin.
data TextAlignment n = BaselineText | BoxAlignedText n n

-- | Make a text from a 'TextAlignment', recommending a fill colour of
--   'black' and 'fontSize' of @'local' 1@.
mkText :: TextAlignment Double -> String -> Diagram V2
mkText a = applyStyle sty . mkText' a
  where
    sty = mempty & backupAttr _FontSize    ?~ local 1
                 & backupAttr _FillTexture ?~ toTexture (SomeColor black)

-- | Make a text from a 'TextAlignment' without any default size or fill
--   colour. This is useful is you want to recommend your own using
--   'recommendFillColor' or 'recommendFontSize'.
mkText' :: TypeableFloat n => TextAlignment n -> String -> QDiagram V2 n Any
mkText' a t = mkQD (Prim $ Text a t)
                   (pointEnvelope origin)
                   mempty
                   mempty

-- | Create a primitive text diagram from the given string, with center
--   alignment, equivalent to @'alignedText' 0.5 0.5@.
--
--   Note that it /takes up no space/, as text size information is not
--   available.
text :: String -> Diagram V2
text = alignedText 0.5 0.5

-- | Create a primitive text diagram from the given string, origin at
--   the top left corner of the text's bounding box, equivalent to
--   @'alignedText' 0 1@.
--
--   Note that it /takes up no space/.
topLeftText :: String -> Diagram V2
topLeftText = alignedText 0 1

-- | Create a primitive text diagram from the given string, with the
--   origin set to a point interpolated within the bounding box.  The
--   first parameter varies from 0 (left) to 1 (right), and the second
--   parameter from 0 (bottom) to 1 (top). Some backends do not
--   implement this and instead snap to closest corner or the center.
--
--   The height of this box is determined by the font's potential ascent
--   and descent, rather than the height of the particular string.
--
--   Note that it /takes up no space/.
alignedText :: Double -> Double -> String -> Diagram V2
alignedText w h = mkText (BoxAlignedText w h)

-- | Create a primitive text diagram from the given string, with the
--   origin set to be on the baseline, at the beginning (although not
--   bounding).  This is the reference point of showText in the Cairo
--   graphics library.
--
--   Note that it /takes up no space/.
baselineText :: String -> Diagram V2
baselineText = mkText BaselineText

------------------------------------------------------------------------
-- Text attributes
------------------------------------------------------------------------

-- Font size -----------------------------------------------------------

-- | The @Font@ attribute specifies the name of a font family.  Inner
--   @Font@ attributes override outer ones.
newtype Font = Font (Last String)
  deriving (Typeable, Semigroup, Eq)

-- | Isomorphism between a font family and its string name.
_Font :: Iso' Font String
_Font = coerced
{-# INLINE _Font #-}

instance AttributeClass Font where
  type AttrType Font = 'IAttr

-- | Specify a font family to be used for all text within a diagram.
font :: ApplyStyle a => String -> a -> a
font = applyAttr _Font

-- | Lens onto the font name of a style.
_font :: HasStyle a => Lens' a (Maybe String)
_font = style . atAttr _Font

-- Font size -----------------------------------------------------------

-- | The @FontSize@ attribute specifies the size of a font's
--   em-square.  Inner @FontSize@ attributes override outer ones.
newtype FontSize n = FontSize (Last n)
  deriving (Typeable, Semigroup, Functor)

-- | Isomorphism between a font size wrapper and the font size.
_FontSize :: Iso' (FontSize n) n
_FontSize = coerced

instance Num n => Default (FontSize n) where
  def = review _FontSize 11

instance Typeable n => AttributeClass (FontSize n) where
  type AttrType (FontSize n) = 'MAttr

-- instance Num n => Default (FontSizeM n) where
--   def = FontSize . Recommend . Last <$> local 1

-- | Set the font size, that is, the size of the font's em-square as
--   measured within the current local vector space. The default size
--   is @local 1@ (which is applied by 'recommendFontSize').
--
--   Note that 'Measure' has a 'Num' instance which uses @'fromIntegral
--   = 'output'@ so writing @'fontSize' 8@ is the same of @'fontSize'
--   ('output' 8)@.
fontSize :: (N a ~ n, Typeable n, ApplyStyle a) => Measure n -> a -> a
fontSize = applyAttr _FontSize

-- | A convenient synonym for 'fontSize (global w)'.
fontSizeG :: (N a ~ n, Typeable n, Num n, ApplyStyle a) => n -> a -> a
fontSizeG = fontSize . global

-- | A convenient synonym for 'fontSize (normalized w)'.
fontSizeN :: (N a ~ n, Typeable n, Num n, ApplyStyle a) => n -> a -> a
fontSizeN = fontSize . normalized

-- | A convenient synonym for @'fontSize' ('output' w)@.
fontSizeO :: (N a ~ n, Typeable n, ApplyStyle a) => n -> a -> a
fontSizeO = fontSize . output

-- | A convenient sysnonym for @'fontSize' ('local' w)@.
fontSizeL :: (N a ~ n, Typeable n, Num n, ApplyStyle a) => n -> a -> a
fontSizeL = fontSize . local

-- | Lens to commit a font size. This is *not* a valid lens (see
--   'commited'.
--
-- @
-- '_fontSize' :: 'Lens'' ('Style' 'V2' 'Double') ('Maybe' ('Measure' n))
-- @
_fontSize :: (InSpace v n a, HasStyle a, Typeable n) => Lens' a (Maybe (Measure n))
_fontSize = style . atAttr _FontSize

-- Font slant ----------------------------------------------------------

-- | The @FontSlantA@ attribute specifies the slant (normal, italic,
--   or oblique) that should be used for all text within a diagram.
--   Inner @FontSlantA@ attributes override outer ones.
data FontSlant
  = FontSlantNormal
  | FontSlantItalic
  | FontSlantOblique
  deriving (Eq, Ord, Typeable, Show)

instance AttributeClass FontSlant where
  type AttrType FontSlant = 'IAttr

instance Semigroup FontSlant where
  _ <> b = b

instance Default FontSlant where
  def = FontSlantNormal

-- | Identity isomorphism, useful for type inference.
_FontSlant :: Equality' FontSlant FontSlant
_FontSlant = id

-- | Specify the slant (normal, italic, or oblique) that should be
--   used for all text within a diagram.  See also 'italic' and
--   'oblique' for useful special cases.
fontSlant :: ApplyStyle a => FontSlant -> a -> a
fontSlant = applyAttr _FontSlant

-- | Lens onto the font slant in a style.
--
-- @
-- '_fontSlant' :: 'Lens'' ('Style' 'V2' 'Double') ('Maybe' 'FontSlant')
-- @
_fontSlant :: HasStyle a => Lens' a (Maybe FontSlant)
_fontSlant = style . atAttr _FontSlant

-- | Set all text in italics.
italic :: ApplyStyle a => a -> a
italic = fontSlant FontSlantItalic

-- | Set all text using an oblique slant.
oblique :: ApplyStyle a => a -> a
oblique = fontSlant FontSlantOblique

-- Font weight ---------------------------------------------------------

-- | The @FontWeightA@ attribute specifies the weight (normal or bold)
--   that should be used for all text within a diagram.  Inner
--   @FontWeightA@ attributes override outer ones.
data FontWeight
  = FontWeightNormal
  | FontWeightBold
  | FontWeightBolder
  | FontWeightLighter
  | FontWeightThin
  | FontWeightUltraLight
  | FontWeightLight
  | FontWeightMedium
  | FontWeightSemiBold
  | FontWeightUltraBold
  | FontWeightHeavy
  deriving (Eq, Ord, Show, Typeable)

instance AttributeClass FontWeight where
  type AttrType FontWeight = 'IAttr

-- | Last semigroup structure
instance Semigroup FontWeight where
  _ <> b = b

instance Default FontWeight where
  def = FontWeightNormal

-- | 'Equality' for a font slant, useful for type inference.
_FontWeight :: Equality' FontWeight FontWeight
_FontWeight = id

-- | Specify the weight (normal, bolder, lighter or bold) that should be
--   used for all text within a diagram.  See also 'bold'
--   for a useful special case.
fontWeight :: ApplyStyle a => FontWeight -> a -> a
fontWeight = applyAttr _FontWeight

-- | Set all text using a bold font weight.
bold :: ApplyStyle a => a -> a
bold = fontWeight FontWeightBold

-- | Set all text using a thin font weight.
thinWeight :: ApplyStyle a => a -> a
thinWeight = fontWeight FontWeightThin

-- | Set all text using a extra light font weight.
ultraLight :: ApplyStyle a => a -> a
ultraLight = fontWeight FontWeightUltraLight

-- | Set all text using a light font weight.
light :: ApplyStyle a => a -> a
light = fontWeight FontWeightLight

-- | Set all text using a medium font weight.
mediumWeight :: ApplyStyle a => a -> a
mediumWeight = fontWeight FontWeightMedium

-- | Set all text using a semi-bold font weight.
semiBold :: ApplyStyle a => a -> a
semiBold = fontWeight FontWeightSemiBold

-- | Set all text using an ultra-bold font weight.
ultraBold :: ApplyStyle a => a -> a
ultraBold = fontWeight FontWeightUltraBold

-- | Set all text using a heavy/black font weight.
heavy :: ApplyStyle a => a -> a
heavy = fontWeight FontWeightHeavy

-- | Set all text to be bolder than the inherited font weight.
bolder :: ApplyStyle a => a -> a
bolder = fontWeight FontWeightBolder

-- | Set all text to be lighter than the inherited font weight.
lighter :: ApplyStyle a => a -> a
lighter = fontWeight FontWeightLighter

-- | Lens onto the font weight in a style.
--
-- @
-- '_fontWeight :: 'Lens'' ('Style' 'V2' 'Double') ('Maybe' 'FontWeight')
-- @
_fontWeight :: HasStyle a => Lens' a (Maybe FontWeight)
_fontWeight = style . atAttr _FontWeight

