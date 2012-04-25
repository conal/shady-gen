{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeOperators #-}
-- {-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.AltNatTrie
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPL-3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
--
-- Tries on functions from typed numbers
----------------------------------------------------------------------

module Data.AltNatTrie (NatFun{-, memo-} ) where

import qualified Data.StableMemo as SM

import Graphics.Shady.Vec (Nat(..),IsNat, Z,S,nat)

type NatFun f = forall n. IsNat n => Nat n -> f n

-- | Represents a function of type NatFun
data NatTrie f n = NT (f n) (NatTrie f (S n))

-- trie :: NatFun f -> forall n. IsNat n => NatTrie f n
-- trie h = NT (h nat) (trie h)

-- trie :: NatFun f -> forall n. IsNat n => Nat n -> NatTrie f n
trie :: NatFun f -> NatFun (NatTrie f)
trie h n = NT (h n) (trie h (Succ n))

-- I don't know how to define untrie

-- untrie :: NatTrie f -> NatFun f
-- untrie (NT x _) Zero     = x
-- untrie (NT _ t) (Succ n) = unO (untrie t n)

-- -- t:: NatTrie (f :. S)
-- -- untrie t :: NatFun (f :. S)
-- -- untrie t :: forall n. Nat n -> (f :. S) n
-- -- unO . untrie t :: forall n. Nat n -> f (S n)

-- memo :: NatFun f -> NatFun f
-- memo h = (SM.memo . untrie . trie) h
--          -- SM.memo $ untrie (trie h)

-- -- TODO: Test with and without the SM.memo.  I expect SM.memo to yield the
-- -- same result but work faster.

-- -- "Quantified type variable `n' escapes":
-- --
-- -- memo = untrie . trie
