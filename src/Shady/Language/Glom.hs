{-# LANGUAGE GADTs, FlexibleContexts, Rank2Types, KindSignatures
           , MultiParamTypeClasses, FunctionalDependencies
           , FlexibleInstances, UndecidableInstances
           , TypeFamilies
           , EmptyDataDecls  -- temporary
  #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Language.Glom
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Typed conglomerate of values
----------------------------------------------------------------------

module Shady.Language.Glom
  ( FunctorU(..), MonadU(..)
  , Glom(..), foldG, mapAG
  -- , Glommable(..), Unglommable(..)
  ) where

import Control.Applicative (Applicative(..),liftA2)

import Text.PrettyPrint.Leijen
import Text.PrettyPrint.Leijen.PrettyPrec
import Text.PrettyPrint.Leijen.DocExpr

import Shady.Language.Type (HasType,PairF(..),UnitF(..))

infixr 7 :*

-- | Map a polymorphic function over a conglomerate (preserving
-- structure).  The required laws are the same as with 'Functor'.
class FunctorU q where
  fmapU :: (forall a.   f a ->   g a)
        -> (forall a. q f a -> q g a)

-- TODO: fill in ApplicativeU

class FunctorU m => MonadU m where
  returnU :: f a -> m f a
  extendU :: (forall a.   f a -> m g a)
          -> (forall a. m f a -> m g a)

-- TODO: does FunctorU already have a name?

-- | A typed conglomerate of values
data Glom f a where
  BaseG :: f a -> Glom f a
  UnitG :: Glom f ()
  (:*)  :: (HasType a, HasType b, Show a, Show b) =>
           Glom f a -> Glom f b -> Glom f (a,b)

instance UnitF (Glom f) where unit = UnitG
instance PairF (Glom f) where (#)  = (:*)

instance FunctorU Glom where
  fmapU h (BaseG x) = BaseG (h x)
  fmapU _ UnitG     = UnitG
  fmapU h (p :* q)  = fmapU h p :* fmapU h q

-- | Applicative/monadic map over a 'Glom'.
mapAG :: Applicative m =>
         (forall a.      f a -> m (     g a)) ->
         (forall a. Glom f a -> m (Glom g a))
mapAG h (BaseG x) = fmap BaseG (h x)
mapAG _ UnitG     = pure UnitG
mapAG h (p :* q)  = liftA2 (:*) (mapAG h p) (mapAG h q)

-- Like the tree/substitution monad
instance MonadU Glom where
  returnU             = BaseG
  extendU h (BaseG x) = h x
  extendU _ UnitG     = UnitG
  extendU h (p :* q)  = extendU h p :* extendU h q

-- | Fold over a 'Glom', given handlers for '(:*)', 'UnitG', and 'BaseG',
-- respectively.
foldG :: (c -> c -> c) -> c -> (forall b. f b -> c)
      -> Glom f a -> c
foldG k e f (a :* b)  = foldG k e f a `k` foldG k e f b
foldG _ e _ UnitG     = e
foldG _ _ f (BaseG x) = f x


-- Convert a type to an 'Expr' for unparsing
instance HasExprU f => HasExprU (Glom f) where
  exprU (BaseG x) = exprU x
  exprU UnitG     = var "()"
  exprU (t :* t') = op InfixR 1 ":*" (exprU t) (exprU t')

instance (HasExpr a, HasExprU f) => HasExpr (Glom f a) where expr = exprU

-- Idea: convert a glom into a Doc glom.

instance (HasExpr a, HasExprU f) => PrettyPrec (Glom f a) where 
  prettyPrec p = prettyPrec p . expr
instance (HasExpr a, HasExprU f) => Pretty     (Glom f a) where 
  pretty       = prettyPrec 0
instance (HasExpr a, HasExprU f) => Show       (Glom f a) where 
  show         = show . pretty


-- Examples:

-- newtype Sink a = Sink { sink :: a -> IO () }

-- type Type      = Glom VectorT
-- type Pat       = Glom V
-- type Sinks     = Glom Sink
-- type UniformsE = Glom E

{-

{--------------------------------------------------------------------
    Composing & decomposing Gloms
--------------------------------------------------------------------}

class Glommable u f a | u f -> a where
  glom :: u -> Glom f a

instance Glommable () f () where glom () = UnitG

instance (Glommable ua f a, Glommable ub f b, HasExpr a, HasExpr b) =>
         Glommable (ua,ub) f (a,b) where
  glom (ua,ub)  = glom ua :* glom ub

-- Template to specialize per f:
-- 
--   instance Glommable (f a) f a where glom = BaseG

class Unglommable u f a | a f -> u where
  unglom :: Glom f a -> u

instance Unglommable () f () where unglom _ = ()

instance (Unglommable ua f a, Unglommable ub f b) =>
         Unglommable (ua,ub) f (a,b) where
  unglom (ga :* gb)  = (unglom ga, unglom gb)
  unglom _ = error "unglom: oops"  -- :(

-- Template to specialize per (non-unit, non-pair) t:
-- 
--  instance Unglommable (f t) f t where unglom = unglomId

-- | Unglom a non-unit, non-pair
unglomId :: Glom f a -> f a
unglomId (BaseG ea)  = ea
unglomId _           = error "unglomId: not BaseG.  wtf?"

instance Unglommable (f Int  ) f Int   where unglom = unglomId
instance Unglommable (f Bool ) f Bool  where unglom = unglomId
instance Unglommable (f Float) f Float where unglom = unglomId

instance Unglommable (f (Vec1 a)) f (Vec1 a) where unglom = unglomId
instance Unglommable (f (Vec2 a)) f (Vec2 a) where unglom = unglomId
instance Unglommable (f (Vec3 a)) f (Vec3 a) where unglom = unglomId
instance Unglommable (f (Vec4 a)) f (Vec4 a) where unglom = unglomId

-}
