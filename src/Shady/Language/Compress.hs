{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE GADTs, KindSignatures #-} -- TEMP

-- TODO: restore warnings
-- {-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
-- Module      :  Graphics.Shady.Language.Compress
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPL-3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Compress expressions.  Replaces equal expressions by pointer-equal
-- expressions.  Uses "Data.StableMemo" so that already-pointer-equal
-- expressions are not repeatedly traversed.
----------------------------------------------------------------------

module Graphics.Shady.Language.Compress (compressE)
  where

import Foreign.Storable (Storable)

import Control.Compose ((:.)(..))

import qualified Data.MemoTrie as M

import Data.PolyStableMemo (memo,(:-->))

import Graphics.Shady.Vec
-- import Graphics.Shady.Misc ((~>))
import Graphics.Shady.Language.Operator (Op(..))
import Graphics.Shady.Language.Type (HasType,Type(..),VectorT(..),IsScalar(),Sampler)
-- import Graphics.Shady.Language.Exp (E(..))


type Unop a = a -> a



{--------------------------------------------------------------------
    Temporarily copied from Exp
--------------------------------------------------------------------}

-- | Variable name
type Id = String

-- | Typed variables
data V a = V Id (Type a)

type VT = forall a. HasType a => Id -> Type a -> V a


infixl 9 :^

-- | Simple expressions (no 'Let').  Statically typed 
data E :: * -> * where
  Op   :: Op a -> E a                   -- ^ operator/constant
  Var  :: V  a -> E a                   -- ^ variable
  (:^) :: (HasType a) =>
          E (a -> b) -> E a -> E b      -- ^ application
  Lam  :: (HasType a, HasType b) =>
          V a -> E b -> E (a -> b)      -- ^ abstraction

-- data Op   :: * -> * where
--     -- Literal
--   Lit     :: Show a => a -> Op a
--     -- Booleans
--   And     :: Op (Binop B1)
--   Or      :: Op (Binop B1)
--   Not     :: IsNat n => Op (Unop (Vec n Bool))

type UnopT f = forall b. HasType b => Unop (f b)

type OpT  = forall a   . HasType a              => Op a -> E a
type VarT = forall a   . HasType a              => V  a -> E a
type AppT = forall a b . (HasType a, HasType b) => E (a -> b) -> E a -> E b
type LamT = forall a b . (HasType a, HasType b) => V a -> E b -> E (a -> b)


-- | Maximize sharing within an expression, as a prelude to sharing-based CSE.
compressE :: UnopT E
compressE e = mCopyE e
 where
   mCopyE, copyE :: UnopT E
   -- Memo version
   mCopyE = memo copyE
   -- Copier, with memo-copied components
   copyE (r :^ s)  = mApp (mCopyE r) (mCopyE s)
   copyE (Lam v b) = mLam (mCopyV v) (mCopyE b)
   copyE (Var v)   = mVar (mCopyV v)
   copyE (Op  o)   = mOp  (mCopyO o)
   
   mCopyO, copyO :: UnopT Op
   mCopyO = memo copyO
   -- Copier, with memo-copied components
--    copyO (Lit a) = mLit (copy a)
--    copyO And     = mAnd
   -- TODO: fill in
   copyO o = o
   
   mCopyV, copyV :: UnopT V
   mCopyV = memo copyV
   copyV (V str ty) = mV (mCopyString str) (mCopyTy ty)

   mCopyString :: Unop String
   mCopyString = M.memo id

   mCopyTy, copyTy :: UnopT Type
   mCopyTy = memo copyTy
   -- copyTy = undefined
   copyTy (VecT    vt) = mVecT vt
   copyTy (SamplerT n) = mSamplerT n
   -- TODO: other types

   -- memoized constructors
   mApp :: AppT
   mApp = memoAT (:^)
   mLam :: LamT
   mLam = memoLT Lam
   mOp  :: OpT
   mOp  = memo Op
   mVar :: VarT
   mVar  = memo Var
   
   mSamplerT :: SamplerTT
   mSamplerT = undefined
               -- memoST SamplerT

   mV :: VT
   mV = memoVT V

   mVecT :: VecTT
   mVecT = undefined

type VecTT = forall n a. (IsNat n, IsScalar a, Storable (Vec n a)) =>
             VectorT n a -> Type (Vec n a)

-- data Type a where
--   VecT     :: (IsNat n, IsScalar a, Storable (Vec n a)) =>
--               VectorT n a -> Type (Vec n a)
--   SamplerT :: IsNat n => Nat n -> Type (Sampler n)
--   UnitT    :: Type ()
--   (:*:)    :: (HasType a, HasType b, Show a, Show b) =>
--               Type a -> Type b -> Type (a ,  b)
--   (:->:)   :: Type a -> Type b -> Type (a -> b)



--    mLit :: -- Oops.  Wrong type.  Id newtype wrapper.
--    mLit = memo Lit
--    mAnd = And
--    -- What about polymorphic operators??
--    copy :: HasType a => a -> a
--    copy = undefined  -- how to make unique?  Map of Type-tagged stable values.


-- TODO: also memoize variables and operators.


{--------------------------------------------------------------------
    Memoization helpers
--------------------------------------------------------------------}

newtype EF a b = EF { unEF :: E (a -> b) }

-- type H' a = forall b. HasType b => EF a b -> E b

type H' a = EF a :--> E

newtype H a = H { unH :: H' a }

-- type AppP = forall a. HasType a => E a -> H a
type AppP = E :--> H


toAppP :: AppT -> AppP
toAppP app = \ ea -> H $ \ (EF eab) -> app eab ea

-- toAppP (fromAppP app') = app'

-- \ ea -> H $ \ (EF eab) -> (fromAppP app') eab ea = app'
-- H $ \ (EF eab) -> (fromAppP app') eab ea = app' ea
-- \ (EF eab) -> (fromAppP app') eab ea = unH (app' ea)
-- (fromAppP app') eab ea = unH (app' ea) (EF eab)
-- fromAppP app' eab ea = unH (app' ea) (EF eab)

fromAppP :: AppP -> AppT
fromAppP app' eab ea = unH (app' ea) (EF eab)


inH :: (H' a -> H' a) -> (H a -> H a)
inH h z = H (h (unH z))

-- "Inferred type is less polymorphic than expected":
-- 
--     inH f = H . f . unH
--     inH = unH ~> H

memoAppP :: AppP -> AppP
-- memoAppP :: (f :--> H) -> (f :--> H)
memoAppP app' = memo (inH memo . app')


memoAT :: AppT -> AppT
memoAT app = fromAppP (memoAppP (toAppP app))

-- Inferred type is less polymorphic than expected:
-- 
--     memoAT = fromAppP . memo . toAppP

-- TODO: generate toAppP and fromAppP together.  Revisit Data.Bijection


-- Lam  :: (HasType a, HasType b) => V a -> E b -> E (a -> b)

type D' a = E :--> EF a

newtype D a = D { unD :: D' a }


-- type LamP = forall a. HasType a => V a -> D a
type LamP = V :--> D

toLamP :: LamT -> LamP
toLamP lam = \ v -> D $ \ eb -> EF (lam v eb)

-- toLamP (fromLamP lam') = lam'
-- \ v -> D $ \ eb -> EF (fromLamP lam' v eb) = lam'
-- D $ \ eb -> EF (fromLamP lam' v eb) = lam' v
-- \ eb -> EF (fromLamP lam' v eb) = unD (lam' v)
-- EF (fromLamP lam' v eb) = unD (lam' v) eb
-- fromLamP lam' v eb = unEF (unD (lam' v) eb)

fromLamP :: LamP -> LamT
fromLamP lam' v eb = unEF (unD (lam' v) eb)


inD :: (D' a -> D' a) -> (D a -> D a)
inD h z = D (h (unD z))

memoLamP :: LamP -> LamP
memoLamP app' = memo (inD memo . app')

memoLT :: LamT -> LamT
memoLT lam = fromLamP (memoLamP (toLamP lam))


-- type VT = forall a. Id -> Type a -> V a

-- type VP = Id -> forall a. HasType a => Type a -> V a
type VP = Id -> Type :--> V

toVP :: VT -> VP
toVP v str = v str

fromVP :: VP -> VT
fromVP = id

memoVP :: VP -> VP
memoVP vp = mem (\ str -> memo (vp str))
 where
   mem :: (Id -> Type :--> V) -> (Id -> Type :--> V)
   mem = M.memo

-- If I use M.memo directly, I get "Quantified type variable `a' escapes"

-- If I use Unop (Id -> Type :--> V), I get "Illegal polymorphic or
-- qualified type: Type :--> V.  Perhaps you intended to use
-- -XImpredicativeTypes".  Even with I use -XImpredicativeTypes !

memoVT :: VT -> VT
memoVT v = fromVP (memoVP (toVP v))


type SamplerTT = forall n. IsNat n => Nat n -> Type (Sampler n)

type k :#-> v = forall n. IsNat n => k n -> v n

{-

-- memoST :: SamplerTT -> SamplerTT

type SamplerTP = Nat :--> (Type :. Sampler)

memoSamplerTP :: SamplerTP -> SamplerTP
memoSamplerTP = undefined

toSamplerTP :: SamplerTT -> SamplerTP
toSamplerTP s n = O (s n)

-- Could not deduce (IsNat a) from the context (HasType a)
-}

-- How to address this last problem?  Maybe a memo variant with IsNat in
-- place of HasType.