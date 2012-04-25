-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.NameM
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
--
-- Name supply monad.  Non-abstract synonym for @State [String]@
----------------------------------------------------------------------

module Data.NameM (NameM, genName, runNameM, allNames) where

import Control.Monad.State

type NameM = State [String]

-- Generate a new variable name
genName :: State [x] x
genName = do x:xs' <- get
             put xs'
             return x

runNameM :: NameM a -> a
runNameM m = evalState m allNames

allNames :: [String]
allNames = map reverse (tail names)
 where
   names = "" : [ c:cs | cs <- names , c <- ['a' .. 'z'] ]
