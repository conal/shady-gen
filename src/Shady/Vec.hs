{-# LANGUAGE TypeFamilies, EmptyDataDecls, TypeOperators
           , GADTs, KindSignatures
           , FlexibleInstances, FlexibleContexts
           , UndecidableInstances
           , ScopedTypeVariables, CPP
           , RankNTypes
  #-}
{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Vec
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Experiment in length-typed vectors
----------------------------------------------------------------------

module Shady.Vec
  (
  -- * Type-level numbers
    Z, S, (:+:), ZeroT, OneT, TwoT, ThreeT, FourT
  -- * Typed natural numbers
  , Nat(..), zero, one, two, three, four
  , withIsNat, natSucc, natIsNat
  , natToZ, natEq, natAdd, (:<:)
  , Index(..), succI, index0, index1, index2, index3
  -- * Vectors
  , Vec(..), IsNat(..), (<+>), indices
  , Zero, One, Two, Three, Four, vElems
  , vec1, vec2, vec3, vec4
  , un1, un2, un3, un4
  , get0, get1, get2, get3
  , get, swizzle
  ) where

import Prelude hiding (foldr,sum)

-- #include "Typeable.h"

import Control.Applicative (Applicative(..),liftA2,(<$>))
import Data.Foldable (Foldable(..),sum)
import Data.Maybe (isJust)
-- import Data.Typeable

import Foreign.Storable
import Foreign.Ptr (Ptr,plusPtr,castPtr)

import Control.Compose (result)

import Data.VectorSpace


import Shady.Misc (Sink)
import Data.Proof.EQ


{--------------------------------------------------------------------
    Type-level numbers
--------------------------------------------------------------------}

data Z                                  -- ^ zero
data S n                                -- ^ successor

-- INSTANCE_TYPEABLE0(Z,zTC ,"Z")
-- INSTANCE_TYPEABLE1(S,sTC ,"S")

infixl 6 :+:

-- | Sum of type-level numbers
type family a :+: b

type instance Z   :+: b = b
type instance S a :+: b = S (a :+: b)

type ZeroT  = Z
type OneT   = S ZeroT
type TwoT   = S OneT
type ThreeT = S TwoT
type FourT  = S ThreeT


{--------------------------------------------------------------------
    Typed natural numbers
--------------------------------------------------------------------}

-- Natural numbers
data Nat :: * -> * where
  Zero :: Nat Z
  Succ :: IsNat n => Nat n -> Nat (S n)

instance Show (Nat n) where show = show . natToZ

withIsNat :: (IsNat n => Nat n -> a) -> (Nat n -> a)
withIsNat p Zero     = p Zero
withIsNat p (Succ n) = p (Succ n)

-- Helper for when we don't have a convenient proof of IsNat n.
natSucc :: Nat n -> Nat (S n)
natSucc = withIsNat Succ 

natIsNat :: Nat n -> (IsNat n => Nat n)
natIsNat Zero     = Zero
natIsNat (Succ n) = Succ n

{-

-- Another approach (also works):

data NatIsNat :: * -> * where
  NatIsNat :: IsNat n' => Nat n' -> (n :=: n') -> NatIsNat n

natIsNat' :: Nat n -> NatIsNat n
natIsNat' Zero     = NatIsNat Zero Refl
natIsNat' (Succ n) = NatIsNat (Succ n) Refl

withIsNat' :: (IsNat n => Nat n -> a) -> (Nat n -> a)
withIsNat' p n = case natIsNat' n of
                   NatIsNat n' Refl -> p n'
-}

-- | Interpret a 'Nat' as an 'Integer'
natToZ :: Nat n -> Integer
natToZ Zero     = 0
natToZ (Succ n) = (succ . natToZ) n

-- | Equality test
natEq :: Nat m -> Nat n -> Maybe (m :=: n)
Zero   `natEq` Zero   = Just Refl
Succ m `natEq` Succ n = liftEq <$> (m `natEq` n)
_      `natEq` _      = Nothing

-- | Sum of naturals
natAdd :: Nat m -> Nat n -> Nat (m :+: n)
Zero   `natAdd` n = n
Succ m `natAdd` n = natSucc (m `natAdd` n)

zero :: Nat ZeroT
zero = Zero

one :: Nat OneT
one = Succ zero

two :: Nat TwoT
two = Succ one

three :: Nat ThreeT
three = Succ two

four :: Nat FourT
four = Succ three


infix 4 :<:

-- | Proof that @m < n@
data m :<: n where
  ZLess :: Z :<: S n
  SLess :: m :<: n -> S m :<: S n

-- data Index :: * -> * where
--   Index :: (n :<: lim) -> Nat n -> Index lim

-- or

-- | A number under the given limit, with proof
data Index lim = forall n. IsNat n => Index (n :<: lim) (Nat n)

instance Eq (Index lim) where
  Index _ n == Index _ n' = isJust (n `natEq` n')

succI :: Index m -> Index (S m)
succI (Index p m) = Index (SLess p) (Succ m)

index0 :: Index (S n)
index0 = Index ZLess Zero

index1 :: Index (S (S n))
index1 = succI index0

index2 :: Index (S (S (S n)))
index2 = succI index1

index3 :: Index (S (S (S (S n))))
index3 = succI index2


{--------------------------------------------------------------------
    Vectors
--------------------------------------------------------------------}

infixr 5 :<

-- | Vectors with type-determined length
data Vec :: * -> * -> * where
  ZVec :: Vec Z a                       -- ^ empty vector
  (:<) :: a -> Vec n a -> Vec (S n) a   -- ^ vector cons

-- INSTANCE_TYPEABLE2(Vec,vecTC ,"Vec")


-- instance Show a => Show (Vec n a) where
--   show ZVec = "ZVec"
--   show (a :< v) = show a ++ " :< " ++ show v

-- | Enumerate the elements of a vector.  See also 'elemsV'
-- vElems :: Vec n a -> [a]
-- vElems ZVec      = []
-- vElems (a :< as) = a : vElems as

-- TODO: Add strictness annotations ("!") to (:<) arguments & compare

vElems :: Vec n a -> [a]
vElems = foldr (:) []

instance Functor (Vec n) where
  fmap _ ZVec     = ZVec
  fmap f (a :< u) = f a :< fmap f u


-- | @n@ a vector length.
class {- Typeable n => -} IsNat n where
  nat    :: Nat n
  pureV  :: a   -> Vec n a
  elemsV :: [a] -> Vec n a
  peekV  :: Storable a => Ptr a -> IO (Vec n a)
  pokeV  :: Storable a => Ptr a -> Sink (Vec n a)

instance IsNat Z where
  nat          = Zero
  pureV _      = ZVec
  elemsV []    = ZVec
  elemsV (_:_) = error "elemsV: too many elements"
  peekV        = const (return ZVec)
  pokeV        = const (const (return ()))

instance IsNat n => IsNat (S n) where
  nat               = Succ nat
  pureV a           = a :< pureV a
  elemsV []         = error "elemsV: too few elements"
  elemsV (a : as)   = a :< elemsV as
 peekV p           =  do a  <- peek p
                         as <- peekV (p `plusPtr` sizeOf a)
                         return (a :< as)
                     -- liftA2 (:<) (peek p) (peekV (succPtr p))
  -- peekV = (liftA2.liftA2) (:<) peek (peekV . succPtr)
  -- TODO: Try these niftier peekV definitions
  pokeV p (a :< as) = do poke p a
                         pokeV (p `plusPtr` sizeOf a) as

-- -- Experiment toward simplifying away the plusPtr calls.
-- succPtr :: forall a. Storable a => Ptr a -> Ptr a
-- succPtr p = p `plusPtr` sizeOf (undefined :: a)


-- TODO: Optimize peekV, pokeV.  For instance, unroll the loop in the
-- dictionary, remove the sizeOf dependence on @a@.

applyV :: Vec n (a -> b) -> Vec n a -> Vec n b
ZVec      `applyV` ZVec      = ZVec
(f :< fs) `applyV` (x :< xs) = f x :< (fs `applyV` xs)

instance IsNat n => Applicative (Vec n) where
  pure  = pureV
  (<*>) = applyV

-- Without -fno-warn-incomplete-patterns above,
-- the previous two instances lead to warnings about non-exhaustive
-- pattern matches, although the other possibilities
-- are type-incorrect.  According to SLPJ:
-- 
--   The overlap warning checker simply doesn't take account of GADTs.
--   There's a long-standing project suggestion to fix this:
--   http://hackage.haskell.org/trac/ghc/wiki/ProjectSuggestions .
--   Perhaps a good GSoc project.

instance Foldable (Vec n) where
  foldr _  b ZVec     = b
  foldr h b (a :< as) = a `h` foldr h b as


infixl 1 <+>
-- | Concatenation of vectors
(<+>) :: Vec m a -> Vec n a -> Vec (m :+: n) a
ZVec     <+> v = v
(a :< u) <+> v = a :< (u <+> v)

-- | Indices under @n@: 'index0' :< 'index1' :< ...
indices :: Nat n -> Vec n (Index n)
indices Zero     = ZVec
indices (Succ n) = index0 :< fmap succI (indices n)

-- TODO: Try reimplementing many Vec functions via foldr.  Warning: some
-- (most?) will fail because they rely on a polymorphic combining function.

-- Convenient nicknames

type Zero  = Vec ZeroT
type One   = Vec OneT
type Two   = Vec TwoT
type Three = Vec ThreeT
type Four  = Vec FourT


vec1 :: a -> One a
vec1 a = a :< ZVec

vec2 :: a -> a -> Two a
vec2 a b = a :< vec1 b

vec3 :: a -> a -> a -> Three a
vec3 a b c = a :< vec2 b c

vec4 :: a -> a -> a -> a -> Four a
vec4 a b c d = a :< vec3 b c d

-- | Extract element
un1 :: One a -> a
un1 (a :< ZVec) = a

-- | Extract elements
un2 :: Two a -> (a,a)
un2 (a :< b :< ZVec) = (a,b)

-- | Extract elements
un3 :: Three a -> (a,a,a)
un3 (a :< b :< c :< ZVec) = (a,b,c)

-- | Extract elements
un4 :: Four a -> (a,a,a,a)
un4 (a :< b :< c :< d :< ZVec) = (a,b,c,d)


{--------------------------------------------------------------------
    Vector space instances
--------------------------------------------------------------------}

instance (IsNat n, Num a) => AdditiveGroup (Vec n a) where
  { zeroV = pure 0; (^+^) = liftA2 (+) ; negateV = fmap negate }

instance (IsNat n, Num a) => VectorSpace (Vec n a) where
  type Scalar (Vec n a) = One a -- note 'One'
  (*^) (s :< ZVec) = fmap (s *)

instance (IsNat n, Num a) => InnerSpace (Vec n a) where
   -- u <.> v = vec1 (sum (liftA2 (*) u v))
   (<.>) = (result.result) (vec1 . sum) (liftA2 (*))


{--------------------------------------------------------------------
    Extract elements
--------------------------------------------------------------------}

-- | General indexing, taking a proof that the index is within bounds.
get :: Index n -> Vec n a -> One a
get (Index ZLess     Zero    ) (a :< _)  = vec1 a
get (Index (SLess p) (Succ m)) (_ :< as) = get (Index p m) as


get0 :: Vec (S n)             a -> One a
get1 :: Vec (S (S n))         a -> One a
get2 :: Vec (S (S (S n)))     a -> One a
get3 :: Vec (S (S (S (S n)))) a -> One a

get0 = get index0
get1 = get index1
get2 = get index2
get3 = get index3


-- | Swizzling.  Extract multiple elements simultaneously.
swizzle :: Vec n (Index m) -> Vec m a -> Vec n a
swizzle ZVec        _ = ZVec
swizzle (ix :< ixs) v = un1 (get ix v) :< swizzle ixs v

{-
-- 'a' :< 'b' :< 'c' :< ZVec
t1 :: Three Char
t1 = elemsV "abc"
     -- 'a' :< 'b' :< 'c' :< ZVec

t2 :: Four (Index ThreeT)
t2 = elemsV [index2, index0 ,index1, index2]

-- 'c' :< 'a' :< 'b' :< 'c' :< ZVec
t3 :: Four Char
t3 = swizzle t2 t1
-}



{--------------------------------------------------------------------
    Some instances.  More in Type.hs
--------------------------------------------------------------------}

instance Eq a => Eq (Vec n a) where
  ZVec    == ZVec    = True
  a :< as == b :< bs = a==b && as==bs

instance Ord a => Ord (Vec n a) where
  ZVec      `compare` ZVec      = EQ
  (a :< as) `compare` (b :< bs) =
    case a `compare` b of
      LT -> LT
      GT -> GT
      EQ -> as `compare` bs


{--------------------------------------------------------------------
    Storage
--------------------------------------------------------------------}

instance (IsNat n, Storable a) => Storable (Vec n a) where
   sizeOf    = const (fromIntegral (natToZ (nat :: Nat n))
                      * sizeOf (undefined :: a))
   alignment = const (alignment (undefined :: a))
   peek      = peekV . castPtr
   poke      = pokeV . castPtr

