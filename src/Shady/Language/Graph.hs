{-# LANGUAGE GADTs, KindSignatures, ExistentialQuantification, Rank2Types #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Language.Graph
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
--
-- Based on a typed variant of Andy Gill's data-reify.  After several
-- tries, I wasn't able to reuse data-reify or my typed variant of it.
-- The problem was that I need my 'HasType' class and 'Type' type, but I
-- couldn't parameterize data-reify by the /class/ 'HasType'.
----------------------------------------------------------------------

module Shady.Language.Graph
  (
  -- * Typed identifiers
    NodeId, Tid(..)
  -- * Graph nodes
  , N(..), mapDeRef
  -- * Bindings
  , Bind(..)
  -- * Graphs
  , Graph(..)
  ) where

import Control.Applicative (Applicative(..),liftA2)

import Shady.Language.Operator
import Shady.Language.Exp


{--------------------------------------------------------------------
    Typed identifiers
--------------------------------------------------------------------}

-- | Node Identifiers
type NodeId = Int

-- | Typed variables
data Tid a = Tid NodeId (Type a)

instance Eq (Tid a) where Tid i _ == Tid j _ = i == j

instance Show (Tid a) where show (Tid i _) = 'x' : show i


{--------------------------------------------------------------------
    Graph nodes
--------------------------------------------------------------------}

data N :: * -> * where
  VN  :: V  a -> N a
  ON  :: Op a -> N a
  App :: (HasType a, HasType b) =>
         Tid (a -> b) -> Tid a -> N b


instance Show (N a) where
  show (VN v)    = unwords ["VN" ,show v]
  show (ON o)    = unwords ["ON" ,show o]
  show (App a b) = unwords ["App",show a,show b]



mapDeRef :: Applicative m
         => (forall a. HasType a => E a -> m NodeId)
         -> (forall a. HasType a => E a -> m (N   a))
mapDeRef _ (Var v)  = pure $ VN v
mapDeRef _ (Op o)   = pure $ ON o
mapDeRef f (u :^ v) = liftA2 App (app f u) (app f v)
                      -- liftA2 App (f u) (f v)
mapDeRef _ Lam{}    = notSupp "Lam"

notSupp :: String -> a
notSupp meth = error $ "mapDeRef on E: "++meth++" not supported"

app :: (Functor m, HasType a) => (E a -> m NodeId) -> E a -> m (Tid a)
app f u = fmap (flip Tid (typeOf1 u)) (f u)


{--------------------------------------------------------------------
    Bindings
--------------------------------------------------------------------}

-- | Binding pair
data Bind = forall a. HasType a => Bind NodeId (N a)

instance Show Bind where
  show (Bind v n) = show v ++" = "++ show n


{--------------------------------------------------------------------
    Graphs
--------------------------------------------------------------------}

-- | Graph, described by bindings and a root variable
data Graph a = Graph [Bind] (Tid a)


instance Show (Graph a) where
  show (Graph netlist start) = "let " ++ show netlist ++ " in " ++ show start
