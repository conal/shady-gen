{-# LANGUAGE TypeOperators, BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.StableMemo
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Memoization based using stable names.  WHNFs keys.
----------------------------------------------------------------------

module Data.StableMemo (memo,memo2,memo3) where

import System.IO.Unsafe (unsafePerformIO)
-- import Debug.Trace (trace)

import Control.Concurrent.MVar
import System.Mem.StableName
import qualified Data.IntMap as I


-- import Shady.Language.Graph
-- import Shady.Language.Operator
-- import Shady.Language.Exp
-- import Shady.Language.Graph


-- Stable names have EQ but not Ord, so they're not convenient for fast
-- maps.  On the other hand, there's 'hashStableName', which generates an
-- 'Int', with rare collisions.  So represent the memo table as an IntMap
-- whose entries are lists of StableName/value pairs.


-- @(k a, v a)@ pair
type StableBind k v = (StableName k, v)

-- Stable map
type k :-> v = I.IntMap [StableBind k v]


-- | Pointer-based memoization.  Evaluates keys to WHNF to improve hit rate.
memo :: (k -> v) -> (k -> v)
memo f = fetch f (unsafePerformIO (newMVar I.empty))

-- | Memoized binary function
memo2 :: (k -> l -> v) -> (k -> l -> v)
memo2 h = memo (memo . h)

-- | Memoized ternary function
memo3 :: (k -> l -> m -> v) -> (k -> l -> m -> v)
memo3 h = memo (memo2 . h)

-- TODO: Make lazy and strict versions.

fetch :: (k -> v) -> MVar (k :-> v) -> (k -> v)

fetch f smv !k = unsafePerformIO $
  do st <- makeStableName k
     modifyMVar smv $ \ sm -> return $
       let h = hashStableName st in
         maybe (let v = f k in (I.insertWith (++) h [(st,v)] sm, v)) -- new
               ((,) sm)                                              -- found
               (I.lookup h sm >>= lookup st)                         -- look

{-
---- tests

sqr :: Num a => a -> a
sqr x = trace ("sqr " ++ show x) $ x*x

t1,t2,t3,t4 :: Int

t1 = sqr 6 + sqr 6
t2 = s + s where s = sqr 6

-- Doesn't reuse 6 in ghci & ghc, but probably does with ghc -O
t3 = sqr' 6 + sqr' 6
 where
   sqr' = memo sqr

-- Works!
t4 = sqr' six + sqr' six
 where
   sqr' = memo sqr
   six  = 6

q :: Integer -> Integer
q = memo sqr

-}
