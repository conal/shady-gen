{-# LANGUAGE TypeOperators, FlexibleContexts, TypeFamilies
           , UndecidableInstances #-}
-- {-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Misc
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Misc useful definitions
----------------------------------------------------------------------

module Shady.Misc
  ( -- argument, result, (~>),
    FMod(..), Frac(..), fmodRF, fracRF, fmodViaFrac, fracViaFmod
  , clamp, clampB, smoothStep
  , Unop,Binop
  , padTo
  , flip1, flip2, flip3, flip4
  , Sink, Action, (>+>), forget
  , R
  -- * Find another home
  , EyePos
  ) where

import Control.Applicative ((<$))

-- From TypeCompose package
import Control.Compose (result)
import Control.Instances ()

import Data.Maclaurin ((:>)(..))  -- For Frac instance
import Data.Boolean

type R = Float

-- | Clamp to a given range
clamp :: Ord a => (a,a) -> a -> a
clamp (lo,hi) = max lo . min hi

-- | Variation on 'clamp', using 'OrdB' instead of 'Ord'
clampB :: (IfB a, OrdB a) => (a,a) -> a -> a
clampB (lo,hi) = maxB lo . minB hi

-- | Smooth, clamped transition
smoothStep :: (Ord a, Num a) => (a,a) -> a -> a
smoothStep loHi val = t*t*(3-2*t) where t = clamp loHi val


-- | Unary transformation (endomorphism)
type Unop  a = a -> a

-- | Binary transformation
type Binop a = a -> a -> a


-- | Pad a string to the given length, adding spaces on the right as needed.
padTo :: Int -> String -> String
padTo n str = str ++ replicate (n - length str) ' '

-- | Move first argument to first place (for style uniformity)
flip1 :: (a -> b) -> (a -> b)
flip1 = id

-- | Move second argument to first place ('flip' synonym for style uniformity)
flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 = flip

-- | Move third argument to first place
flip3 :: (a -> b -> c -> d) -> (c -> a -> b -> d)
flip3 = flip . result flip

-- | Move fourth argument to first place
flip4 :: (a -> b -> c -> d -> e) -> (d -> a -> b -> c -> e)
flip4 = flip . result flip3


{--------------------------------------------------------------------
    frac & fmod
--------------------------------------------------------------------}

-- | Take fractional component(s).  Always non-negative.  You can use
-- 'fracRF' for 'RealFrac' types and 'fracViaFmod' for 'Fmod' types.
class Frac a where frac :: a -> a

-- | Real-valued modulo.  You can use 'fmodRF' for 'RealFrac' types and
-- 'fmodViaFrac' for 'Frac' types.
class FMod a where fmod :: a -> a -> a

-- | Fractional component.  Useful for defining 'frac' on 'RealFrac' types.
fracRF :: RealFrac a => a -> a
fracRF x = x - fromIntegral (floor x :: Int)


-- | Fractional modulo.  Useful for defining 'fmod' on 'RealFrac' types.
fmodRF :: RealFrac a => a -> a -> a
x `fmodRF` y = x - y * fromIntegral (floor (x/y) :: Int)

-- | Handy defining 'frac' on a 'FMod' type.
fracViaFmod :: (Num a, FMod a) => a -> a
fracViaFmod = (`fmod` 1)

-- | Handy defining 'fmod' on a 'Frac' type.
fmodViaFrac :: (Fractional a, Frac a) => a -> a -> a
x `fmodViaFrac` y = frac (x/y) * y

instance FMod Float where fmod = fmodRF
instance Frac Float where frac = fracRF

-- 'frac' of a derivative tower is 'frac' of the value and unchanged
-- derivatives.  Not quite right, since 'frac' introduces discontinuities,
-- so all-sided derivatives don't really exist at those points.
instance Frac s => Frac (u :> s) where
  frac (D s l) = D (frac s) l


{--------------------------------------------------------------------
    Information sinks
--------------------------------------------------------------------}

-- | Synonym for @IO ()@.  Obviates some parentheses.
type Action = IO ()

-- | Sink of information
type Sink a = a -> Action

infixr 1 >+>

-- | Combine sinks
(>+>) :: Sink a -> Sink b -> Sink (a,b)
(sa >+> sb) (a,b) = sa a >> sb b

-- | Discard a functor value.
forget :: Functor f => f a -> f ()
forget = (() <$)
-- forget = fmap (const ())

{--------------------------------------------------------------------
    Find another home
--------------------------------------------------------------------}

type EyePos = (R,R,R)

