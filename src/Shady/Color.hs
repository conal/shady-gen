{-# LANGUAGE TypeSynonymInstances, TypeOperators, FlexibleInstances
           , GeneralizedNewtypeDeriving, TypeFamilies
           , MultiParamTypeClasses, UndecidableInstances
  #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Color
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Simple colors
----------------------------------------------------------------------

module Shady.Color
  (
  -- * Basics
    Color, colorToR4, r4ToColor, rgba, rgb, colorR, colorG, colorB, colorA
  -- * Color operations
  , overC, over
  -- * Some colors
  , black, white, red, green, blue, clear, grey, gray
  -- * Conversion to color
  , HasColor(..)
  ) where

import Data.Monoid (Monoid(..))
import Control.Applicative (liftA2)

import Control.Compose ((~>))

import Data.VectorSpace
import Data.NumInstances ()

import Data.Boolean

import Shady.Misc (Unop, Binop)
import Shady.Language.Exp


-- TODO: Vector space instance


{--------------------------------------------------------------------
    Basics
--------------------------------------------------------------------}

type Float4E = (FloatE,FloatE,FloatE,FloatE)

-- | Color, as RGBA
newtype Color = C { unC :: Float4E }
  deriving ( Eq,Ord,Show,Num,Fractional,Floating
           , AdditiveGroup, InnerSpace )

-- VectorSpace has an associated type, which @deriving@ currently doesn't handle.
instance VectorSpace Color where
  type Scalar Color = Scalar (Float4E)
  (*^) s = inC ((*^) s)

-- | Representation conversion
colorToR4 :: Color -> R4E
colorToR4 (C (r,g,b,a)) = vec4 r g b a

-- | Representation conversion
r4ToColor :: R4E -> Color
r4ToColor = C . un4

inC :: Unop (Float4E) -> Unop Color
inC = unC ~> C

inC2 :: Binop (Float4E) -> Binop Color
inC2 = unC ~> inC

-- | Color from red, green, blue, alpha components
rgba :: R1 :=> R1 :=> R1 :=> R1 :=> Color
rgba r g b a = C (r,g,b,a)

-- | Color from red, green, blue components
rgb :: R1 :=> R1 :=> R1 :=> Color
rgb r g b = rgba r g b 1


-- | Extract the red component
colorR :: Color -> FloatE
colorR (C (r,_,_,_)) = r

-- | Extract the green component
colorG :: Color -> FloatE
colorG (C (_,g,_,_)) = g

-- | Extract the blue component
colorB :: Color -> FloatE
colorB (C (_,_,b,_)) = b

-- | Extract the alpha component
colorA :: Color -> FloatE
colorA (C (_,_,_,a)) = a


{--------------------------------------------------------------------
    Color operations
--------------------------------------------------------------------}

-- | Overlay on two colors
overC :: Binop Color
overC top bot = top ^+^ (1 - colorA top) *^ bot

-- | Pointwise 'overC', e.g., for images.
over :: Binop (p->Color)
over = liftA2 overC


{--------------------------------------------------------------------
    Some colors
--------------------------------------------------------------------}

-- | Some colors
black, white, red, green, blue, clear :: Color
black = grey 0
white = grey 1

red   = rgb 1 0 0
green = rgb 0 1 0
blue  = rgb 0 0 1

clear = rgba 0 0 0 0

-- | Shade of grey
grey, gray :: R1 :=> Color
grey x = rgb x x x
gray = grey


{--------------------------------------------------------------------
    Instances
--------------------------------------------------------------------}

instance Monoid Color where
  mempty  = clear
  mappend = overC

instance IfB BoolE Color where
  ifB = inC2 . ifB

{--------------------------------------------------------------------
    Conversion to color
--------------------------------------------------------------------}

class HasColor a where toColor :: a -> Color

instance HasColor Color where toColor = id
instance HasColor BoolE where toColor = boolean nonWhite white

nonWhite :: Color
nonWhite = clear
  -- rgb 0 0.2 0
  -- dark green
  -- clear -- black
