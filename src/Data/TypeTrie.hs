{-# LANGUAGE GADTs, KindSignatures, Rank2Types, TypeOperators #-}

-- Restore later:
{-# OPTIONS_GHC -Wall #-}
-- Remove later:
-- {-# OPTIONS_GHC -fno-warn-missing-fields #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.TypeTrie
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPL-3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Memoization of functions on typed types
----------------------------------------------------------------------

module Data.TypeTrie (memo, memoTy) where


import qualified Data.StableMemo as SM
-- import Data.PolyStableMemo ((:-->))

import Control.Compose ((:.)(..))

import Graphics.Shady.Misc (result)
import Data.NatTrie (NatFun)
import qualified Data.NatTrie as N
import Graphics.Shady.Vec
import Graphics.Shady.Language.Type (Type(..),Sampler,HasType(..),IsScalar,VectorT(..),ScalarT(..))
import Graphics.Shady.Language.Exp (E(..))

type TyFun f = forall a. HasType a => Type a -> f a

{-
data Type :: * -> * where
  VecT     :: (IsNat n, IsScalar a) =>
              VectorT n a -> Type (Vec n a)
  SamplerT :: IsNat n => Nat n -> Type (Sampler n)
  UnitT    :: Type ()
  (:*:)    :: (HasType a, HasType b) =>
              Type a -> Type b -> Type (a ,  b)
  (:->:)   :: Type a -> Type b -> Type (a -> b)
-}

type BinT (#) f = forall a b. (HasType a, HasType b) =>
                  Type a -> Type b -> f (a # b)

data TyTrie f = TyTrie {
    vecTr     :: forall n a. (IsNat n, IsScalar a) =>
                 VectorT n a -> f (Vec n a)
  , samplerTr :: forall n. IsNat n => Nat n -> f (Sampler n)
  , unitTr    :: f ()
  , pairTr    :: BinT (, ) f
  , funTr     :: BinT (->) f
  }

tyTrie :: TyFun f -> TyTrie f

tyTrie h = TyTrie { vecTr     = memoVecT     $ result          h VecT
                  , samplerTr = memoSamplerT $ result          h SamplerT
                  , unitTr    = id           $ id              h UnitT
                  , pairTr    = memoBinT     $ (result.result) h (:*:)
                  , funTr     = memoBinT     $ (result.result) h (:->:)
                  }

-- tyTrie h = TyTrie { vecTr     = memoVecT     $ \ vt    -> h (VecT    vt)
--                   , samplerTr = memoSamplerT $ \ n     -> h (SamplerT n)
--                   , unitTr    =                           h UnitT
--                   , pairTr    = memoBinT     $ \ ta tb -> h (ta :*:  tb)
--                   , funTr     = memoBinT     $ \ ta tb -> h (ta :->: tb)
--                   }

-- I'd like to factor out the memoBinT pattern above.  Types get tricky:
-- 
--   Could not deduce (HasType ((#) a1 b1))
--    from the context (HasType a1, HasType b1)
-- 
-- I don't know how to give an adequate type for (#).

tyUntrie :: TyTrie f -> TyFun f
tyUntrie (TyTrie {vecTr     = v}) (VecT     t) = v t
tyUntrie (TyTrie {samplerTr = s}) (SamplerT n) = s n
tyUntrie (TyTrie {unitTr    = u}) UnitT        = u
tyUntrie (TyTrie {pairTr    = p}) (ta :*:  tb) = p ta tb
tyUntrie (TyTrie {funTr     = f}) (ta :->: tb) = f ta tb

memo, memoTy :: TyFun f -> TyFun f
memoTy h = SM.memo $
           tyUntrie (tyTrie h)

-- memoTy h = SM.memo . tyUntrie . tyTrie $ h

-- TODO: Test with and without the SM.memo.  I expect SM.memo to yield the
-- same result but work faster.

memo = memoTy

newtype P (#) f a = P { unP :: TyFun (f :. (#) a) }

type BinP (#) f = TyFun (P (#) f)

toBinP :: BinT (#) f -> BinP (#) f
-- toBinP (#) bin tya = P (\ tyb -> O (bin tya tyb))
toBinP binT tya = P (O . binT tya)

toBinT :: BinP (#) f -> BinT (#) f
toBinT binP tya tyb = unO (unP (binP tya) tyb)

--           binP           :: BinP (#) f
--           binP           :: TyFun (P f)
--           binP           :: forall a. HasType a => Type a -> P f a
--           binP tya       :: P f a
-- unH (     binP tya)      :: TyFun (f :. (#) a)
-- unH (     binP tya)      :: forall b. HasType b => Type b -> (f :. (#) a) b
-- unH (     binP tya) tyb  :: (f :. (#) a) b
-- unO (unH (binP tya) tyb) :: f (a # b)


memoBinP :: BinP (#) f -> BinP (#) f
memoBinP h = memoTy (\ ta -> P (memoTy (unP (h ta))))

-- Oops.  Each call to memoTy generates a distinct trie.
-- There will be no sharing between them.
-- Is sharing important here?

memoBinT :: BinT (#) f -> BinT (#) f
memoBinT h = toBinT (memoBinP (toBinP h))

type SamplerTT f = forall n. IsNat n => Nat n -> f (Sampler n)

-- type NatFun f = forall n. IsNat n => Nat n -> f n

type SamplerTP f = NatFun (f :. Sampler)

toSamplerTP :: SamplerTT f -> SamplerTP f
-- toSamplerTP h n = O (h n)
toSamplerTP h = O . h

toSamplerTT :: SamplerTP f -> SamplerTT f
-- toSamplerTT h n = unO (h n)
toSamplerTT h = unO . h


memoSamplerT :: SamplerTT f -> SamplerTT f
memoSamplerT h = toSamplerTT (N.memo (toSamplerTP h))

-- memoSamplerT h = unO . N.memo (O . h)



{--------------------------------------------------------------------
    Scalar types
--------------------------------------------------------------------}

-- data ScalarT :: * -> * where
--   Bool  :: ScalarT Bool
--   Int   :: ScalarT Int
--   Float :: ScalarT Float

data ScTrie f = ScTrie {
    boolTr  :: f Bool
  , intTr   :: f Int
  , floatTr :: f Float
  }

type ScFun f = forall a. IsScalar a => ScalarT a -> f a

scTrie :: ScFun f -> ScTrie f
scTrie h = ScTrie {
                 boolTr  = h Bool
               , intTr   = h Int
               , floatTr = h Float
               }

scUntrie :: ScTrie f -> ScFun f
scUntrie (ScTrie { boolTr  = b}) Bool  = b
scUntrie (ScTrie { intTr   = i}) Int   = i
scUntrie (ScTrie { floatTr = f}) Float = f

scMemo :: ScFun f -> ScFun f
scMemo h = scUntrie (scTrie h)


{--------------------------------------------------------------------
    Vector types
--------------------------------------------------------------------}

--   VecT     :: (IsNat n, IsScalar a, Storable (Vec n a)) =>
--               VectorT n a -> Type (Vec n a)

-- data VectorT n a = VectorT (Nat n) (ScalarT a)

-- TODO: add constraints
type VecFun f = forall n a. (IsNat n, IsScalar a) => VectorT n a -> f (Vec n a)

data SF f n = SF { unSF :: ScFun (f :. Vec n) }

type VecP f = NatFun (SF f)

memoVecP :: VecP f -> VecP f
memoVecP h = N.memo (\ n -> SF (scMemo (unSF (h n))))

toVecP :: VecFun f -> VecP f
toVecP h = \ n -> SF (\ a -> O (h (VectorT n a)))

{-
                                  n       :: Nat n
                                    a     :: ScalarT a
                          VectorT n a     :: VectorT n a
                       h (VectorT n sc)   :: f (Vec n a)
                    O (h (VectorT n sc))  :: (f :. Vec n) a
             \ a -> O (h (VectorT n sc))  :: ScFun (f :. Vec n)
         SF (\ a -> O (h (VectorT n sc))) :: SF f n
  \ n -> SF (\ a -> O (h (VectorT n sc))) :: NatFun (SF f)
  \ n -> SF (\ a -> O (h (VectorT n sc))) :: VecP f
-}

toVecT :: VecP f -> VecFun f
toVecT h = \ (VectorT n a) -> unO (unSF (h n) a)

memoVecT :: VecFun f -> VecFun f
memoVecT h = toVecT (memoVecP (toVecP h))


{--------------------------------------------------------------------
    Memoizing identity -- hash re-consing
--------------------------------------------------------------------}

-- newtype Endo f a = Endo (f a -> f a)

-- compressE :: TyFun (Endo E)
-- compressE = memoTy $ 

compressTy :: () -> TyFun Type
compressTy () = mCopyTy
 where
   mCopyTy = memoTy copyTy
   copyTy 