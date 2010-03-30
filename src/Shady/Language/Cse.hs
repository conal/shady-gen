{-# LANGUAGE GADTs, KindSignatures, TypeFamilies, MultiParamTypeClasses
           , ScopedTypeVariables, PatternGuards
  #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-imports -fno-warn-orphans -fno-warn-missing-signatures #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Language.Cse
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Common subexpression elimination.
-- 
-- TODO: Improve variable names (now \"x8\" etc).
----------------------------------------------------------------------

module Shady.Language.Cse (cse) where

import Control.Applicative (pure,(<$>),(<*>))
import Data.Maybe (fromMaybe)
import qualified Data.IntMap as I

import System.IO.Unsafe (unsafePerformIO)

import Shady.Misc
import Shady.Language.Type
import Shady.Language.Operator
import Shady.Language.Exp

import Shady.Language.Graph
import Shady.Language.Reify

-- V from Tid
ev :: Tid a -> V a
ev (Tid i t) = V ('x':show i) t

children :: N a -> [NodeId]
children (VN  _)   = []
children (ON  _)   = []
children (App (Tid a _) (Tid b _)) = [a,b]

childrenB :: Bind -> [NodeId]
childrenB (Bind _ n) = children n

-- Number of references for each node.  Important: partially apply, so
-- that the binding list can be converted just once into an efficiently
-- searchable representation.
uses :: [Bind] -> (NodeId -> Int)
uses = fmap (fromMaybe 0) .
       flip I.lookup .
       histogram .
       concatMap childrenB

-- histogram :: Ord k => [k] -> I.Map k Int
-- histogram = foldr (\ k -> I.insertWith (+) k 1) I.empty

histogram :: [Int] -> I.IntMap Int
histogram = foldr (\ k -> I.insertWith (+) k 1) I.empty

-- Fast version, using an IntMap.  Important: partially apply.
bindsF :: forall a. [Bind] -> (Tid a -> N a)
bindsF binds = \ (Tid i' a') -> extract a' (I.lookup i' m)
 where
   m :: I.IntMap Bind
   m = I.fromList [(i,b) | b@(Bind i _) <- binds]
   extract :: Type a' -> Maybe Bind -> N a'
   extract _ Nothing            = error "bindsF: variable not found"
   extract a' (Just (Bind _ n))
     | Just Refl <- typeOf1 n `tyEq` a' = n
     | otherwise                        =
         error $ "bindsF: wrong type.  " ++ show (typeOf1 n) ++ " vs " ++ show a'

tid :: HasType a => NodeId -> Tid a
tid i = Tid i typeT

letI :: (HasType a, HasType b) => NodeId -> E a -> E b -> E b
letI i = letE (ev (tid i))

unGraph :: HasType a => Graph a -> E a
unGraph (Graph binds root) = foldr llet (var' root) (reverse binds)
 where
   -- Wrap a let if non-trivial
   llet :: HasType b => Bind -> E b -> E b
   llet bind | trivial bind = id
   llet (Bind i n)          = letI i (nodeE' n)
   -- How many uses of variable
   count :: NodeId -> Int
   count = uses binds
   -- Bindings as IntMap lookup
   psf :: Tid a -> N a
   psf = bindsF binds
   -- Too trivial to bother abstracting.
   trivial :: Bind -> Bool
   trivial (Bind _ (VN _))          = True
   trivial (Bind _ (ON (Lit a)))    = not (abstractable a)
   trivial (Bind _ (ON _))          = True
   trivial (Bind i _) | count i < 2 = True
   trivial _                        = False
   -- Like nodeE but with inlining of trivial bindings
   nodeE' :: N a -> E a
   nodeE' (VN v)    = Var v
   nodeE' (ON o)    = Op o
   nodeE' (App a b) = var' a :^ var' b
   -- Variable reference or inline
   var' :: HasType a => Tid a -> E a
   var' t@(Tid i _) | trivial (Bind i n) = nodeE' n
                    | otherwise          = Var (ev t)
    where
      n = psf t

-- Possible and worthwhile to abstract.
abstractable :: forall a. HasType a => a -> Bool
abstractable a = 
   case (typeOf a :: Type a) of
     VecT (VectorT n _) -> natToZ n > 1
     _                  -> False

-- | Common subexpression elimination.  Use with care, since it breaks
-- referential transparency on the /representation/ of expressions, but
-- not on their meaning.
cse :: HasType a => E a -> E a
cse = unsafePerformIO . fmap unGraph . reifyGraph

{-

-- Remove the comment braces to use the testing code

{--------------------------------------------------------------------
    Testing
--------------------------------------------------------------------}

-- Simpler version of unGraph.  No inlining.
unGraph' :: HasType a => Graph a -> E a
unGraph' (Graph binds root) = foldr f (Var (ev root)) (reverse binds)
 where
   f :: Bind -> (forall b. HasType b => E b -> E b)
   f (Bind i n) = letE (ev (Tid i (typeOf1 n))) (nodeE n)
   nodeE (VN v)    = Var v
   nodeE (ON o)    = Op o
   nodeE (App u v) = Var (ev u) :^ Var (ev v)

-- Convert expressions to simple SSA forms
ssa :: HasType a => E a -> IO (E a)
ssa = fmap unGraph' . reifyGraph


-- type-specialize
reify :: HasType a => E a -> IO (Graph a)
reify = reifyGraph

type I1 = One Int

va, vb :: E I1
va = Var (var "a")
vb = Var (var "b")


-- test expressions
e1 = va + vb :: E I1
e2 = e1 * e1
e3 = va + va :: E I1

-- For instance,


-- > e2
-- (a + b) * (a + b)
-- 
-- > reify e2
-- let [0 = App x1 x3,1 = App x2 x3,3 = App x4 x7,7 = VN b,4 = App x5 x6,6 = VN a,5 = ON (+),2 = ON (*)] in x0
-- 
-- > ssa e2
-- let x2 = (*) in 
--   let x5 = (+) in 
--     let x6 = a in 
--       let x4 = x5 x6 in 
--         let x7 = b in 
--           let x3 = x4 x7 in 
--             let x1 = x2 x3 in 
--               let x0 = x1 x3 in 
--                 x0
-- 
-- > cse e2
-- let x3 = a + b in 
--   x3 * x3


-}
