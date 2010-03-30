{-# LANGUAGE TypeOperators, BangPatterns, Rank2Types, PatternGuards
           , ExistentialQuantification, ScopedTypeVariables, GADTs
  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}  -- temp
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.PolyStableMemo
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Polymorphic memoization based using stable names.
----------------------------------------------------------------------

module Data.PolyStableMemo ((:-->),memo) where -- ,memo2,memo3

import System.IO.Unsafe (unsafePerformIO)

import Control.Concurrent.MVar
import System.Mem.StableName
import qualified Data.IntMap as I

-- import Shady.Language.Graph
-- import Shady.Language.Operator
-- import Shady.Language.Exp
-- import Shady.Language.Graph

import Shady.Language.Type


-- Stable names have EQ but not Ord, so they're not convenient for fast
-- maps.  On the other hand, there's 'hashStableName', which generates an
-- 'Int', with rare collisions.  So represent the memo table as an IntMap
-- whose entries are lists of StableName/value pairs.


-- @(k a, v a)@ pair
data StableBind k v =
  forall a. HasType a => SB (StableName (k a)) (v a)

-- Polymorphic function
type k :--> v = forall a. (HasType a, Show a) => k a -> v a

-- Sorry about the 'Show' constraint.  Turns out to be needed indirectly,
-- due to constant folding.

-- Stable map
type SM k v = I.IntMap [StableBind k v]

-- | Pointer-based memoization.  Evaluates keys to WHNF to improve hit rate.
memo :: (k :--> v) -> (k :--> v)
-- memo :: (forall a. HasType a => k a -> v a) -> (forall a. HasType a => k a -> v a)
-- memo :: HasType a => (k a -> v a) -> (k a -> v a)
memo f = fetch f (unsafePerformIO (newMVar I.empty))

{-
-- polymorphic function
newtype Pfun p q a = Pfun { unPfun :: p a -> q a }

-- | Memoized binary function
-- memo2 :: HasType a =>
--          (k a -> l a -> v a) -> (k a -> l a -> v a)
memo2 :: (forall a. HasType a => k a -> l a -> v a)
      -> (forall a. HasType a => k a -> l a -> v a)
memo2 h = unPfun . memo (Pfun . memo . h)

-- h                               :: k a -> l a -> v a
-- memo . h                        :: k a -> l a -> v a
-- Pfun . memo . h                 :: k a -> Pfun l v a
-- memo (Pfun . memo . h)          :: k a -> Pfun l v a
-- unPfun . memo (Pfun . memo . h) :: k a -> l a -> v a

-- | Memoized binary function
memo3 :: HasType a =>
         (k a -> l a -> m a -> v a) -> (k a -> l a -> m a -> v a)
memo3 h = unPfun . memo (Pfun . memo2 . h)

pfun2 :: (l a -> m a -> v a) -> Pfun l (Pfun m v) a
pfun2 = Pfun . fmap Pfun

unPfun2 :: Pfun l (Pfun m v) a -> (l a -> m a -> v a)
unPfun2 = fmap unPfun . unPfun

-- h                                  :: k a -> l a -> m a -> v a
-- memo2 . h                          :: k a -> l a -> m a -> v a
-- pfun2 . memo2 . h                  :: k a -> Pfun l (Pfun m v) a
-- memo (pfun2 . memo2 . h)           :: k a -> Pfun l (Pfun m v) a
-- unPfun2 . memo (pfun2 . memo2 . h) :: k a -> l a -> m a -> v a

-- I worry that the function compositions will lose sharing.


-- -- | Memoized ternary function
-- memo3 :: HasType a =>
--          (k a -> l a -> m a -> v a) -> (k a -> l a -> m a -> v a)
-- memo3 h = unPfun2 . memo (pfun2 . memo2 . h)

-}

-- TODO: Make lazy and strict versions.

-- fetch :: (k :--> v) -> MVar (SM k v) -> (k :--> v)
fetch :: HasType a => (k a -> v a) -> MVar (SM k v) -> (k a -> v a)

fetch f smv !k = unsafePerformIO $
  do st <- makeStableName k
     modifyMVar smv $ \ sm -> return $
       let h = hashStableName st in
         maybe (let v = f k in (I.insertWith (++) h [SB st v] sm, v)) -- new
               ((,) sm)                       -- found
               (I.lookup h sm >>= blookup st) -- look

blookup :: forall k v a. HasType a =>
           StableName (k a) -> [StableBind k v] -> Maybe (v a)
blookup stk = look
 where
   look :: [StableBind k v] -> Maybe (v a)
   look [] = Nothing
   look (SB stk' v : binds') 
     | Just Refl <- tya `tyEq` typeOf2 stk', stk == stk' = Just v
     | otherwise                                         = look binds'
   tya :: Type a
   tya = typeT
