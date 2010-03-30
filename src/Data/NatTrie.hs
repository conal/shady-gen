{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.NatTrie
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPL-3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Tries on functions from typed numbers
----------------------------------------------------------------------

module Data.NatTrie (NatFun, memo, nats, canonicalNat) where

import Control.Compose ((:.)(..))

import qualified Data.StableMemo as SM

import Graphics.Shady.Vec (Nat(..),IsNat, Z,S)

type NatFun f = forall n. IsNat n => Nat n -> f n

-- | Represents a function of type @NatFun f@
data NatTrie f = NT (f Z) (NatTrie (f :. S))

trie :: NatFun f -> NatTrie f
trie h = NT (h Zero) (trie (O . h . Succ))

--           h         :: forall n. Nat n -> f n
--           h . Succ  :: forall n. Nat n -> f (S n)
--       O . h . Succ  :: forall n. Nat n -> (f :. S) n
-- trie (O . h . Succ) :: NatTrie (f :. S)

untrie :: NatTrie f -> NatFun f
untrie (NT x _) Zero     = x
untrie (NT _ t) (Succ n) = unO (untrie t n)

-- t:: NatTrie (f :. S)
-- untrie t :: NatFun (f :. S)
-- untrie t :: forall n. Nat n -> (f :. S) n
-- unO . untrie t :: forall n. Nat n -> f (S n)

memo :: NatFun f -> NatFun f
memo h = (SM.memo . untrie . trie) h
         -- SM.memo $ untrie (trie h)
         -- SM.memo h

-- TODO: Test with and without the SM.memo.  I expect SM.memo to yield the
-- same result but work faster.

-- Hm.  Why is SM.memo sufficient (as opposed to PSM.memo)?  Am I missing something?
-- Even the following type-checks:

memo' :: NatFun f -> NatFun f
memo' h = SM.memo h

-- but not memo' = SM.memo.  What's going on?

-- SM.memo h ::

-- "Quantified type variable `n' escapes":
-- 
-- memo = untrie . trie 

nats :: NatTrie Nat
nats = trie id

canonicalNat :: NatFun Nat
canonicalNat = memo id

-- 