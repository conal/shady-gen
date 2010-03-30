{-# LANGUAGE UndecidableInstances, TypeFamilies, BangPatterns, Rank2Types
           , ExistentialQuantification, PatternGuards, ScopedTypeVariables
           , MultiParamTypeClasses, GADTs
  #-}
{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
-- Module      :  Shady.Language.Reify
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Discover representation sharing in expressions
-- Variation on Andy Gill's Data.Reify.
----------------------------------------------------------------------



module Shady.Language.Reify (reifyGraph) where

import Control.Concurrent.MVar
-- import Control.Monad
import System.Mem.StableName
import Data.IntMap as M

import Shady.Language.Exp
import Shady.Language.Graph


data StableBind = forall a. HasType a => StableBind NodeId (StableName (E a))


-- | 'reifyGraph' takes a data structure that admits 'MuRef', and returns
-- a 'Graph' that contains the dereferenced nodes, with their children as
-- integers rather than recursive values.
reifyGraph :: HasType a => E a -> IO (Graph a)
reifyGraph e = do rt1   <- newMVar M.empty
                  rt2   <- newMVar []
                  root  <- findNodes rt1 rt2 e
                  binds <- readMVar rt2
                  return (Graph binds (Tid root typeT))


findNodes :: HasType a =>
             MVar (IntMap [StableBind])
          -> MVar [Bind]
          -> E a -> IO NodeId
findNodes rt1 rt2 ea =
  do nextI <- newMVar 0
     let newIndex = modifyMVar nextI (\ n -> return (n+1,n))
         loop :: HasType b => E b -> IO NodeId
         loop !eb = do
               st  <- makeStableName eb
               tab <- takeMVar rt1
               case mylookup st tab of
                 Just i -> do putMVar rt1 tab
                              return $ i
                 Nothing -> 
                   do i <- newIndex
                      putMVar rt1 $
                        M.insertWith (++) (hashStableName st) [StableBind i st] tab
                      res  <- mapDeRef loop eb
                      tab' <- takeMVar rt2
                      putMVar rt2 $ Bind i res : tab'
                      return i
       in loop ea

mylookup :: forall a. HasType a =>
            StableName (E a) -> IntMap [StableBind] -> Maybe NodeId
mylookup sta tab =
   M.lookup (hashStableName sta) tab >>= llookup
 where
   tya :: Type a
   tya = typeT
   llookup :: [StableBind] -> Maybe NodeId
   llookup [] = Nothing
   llookup (StableBind i stb : binds') 
     | Just Refl <- tya `tyEq` typeOf2 stb, sta == stb = Just i
     | otherwise                                       = llookup binds'
