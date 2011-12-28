{-# LANGUAGE GADTs, KindSignatures, FlexibleInstances, FlexibleInstances
           , MultiParamTypeClasses, FunctionalDependencies
           , UndecidableInstances, TypeOperators, ScopedTypeVariables
           , FlexibleContexts, CPP
  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

----------------------------------------------------------------------
-- |
-- Module      :  Shady.Language.Type
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Types
----------------------------------------------------------------------

module Shady.Language.Type
  ( 
  -- * Type values
    ScalarT(..), VectorT(..), Type(..)
  , TextureId, Sampler(..), sampler1, sampler2, sampler3, Sampler1, Sampler2, Sampler3
  -- * Generating type values
  , IsScalar(..), vectorT, HasType(..)
  , typeOf, typeOf1, typeOf2, compatible, compatible1
  , IsVec(..),checkVec, checkVec'
  -- * Type equality
  , (:=:)(..), ptyEq, vtyEq, tyEq
  , (=:=), (===)
  -- * Vector operations
  -- , Vector(..)
  -- * Convenient type synonyms
  , R, R1, R2, R3, R4, B1, Pred1, Pred2
  -- * Notions of equality
  , SynEq(..),SynEq2(..) 
  -- * Pairing and unit
  , PairF(..), (:#), UnitF(..)
  -- * Re-export
  , module TypeUnary.Vec
  ) where

import Control.Applicative (pure,liftA2,Const(..))
import Data.Maybe (isJust)
import Data.Foldable (toList)
-- import Data.List (intercalate)
import Control.Monad.Instances ()
import Foreign.Storable

import Data.Typeable (Typeable)

import Text.PrettyPrint.Leijen
import Text.PrettyPrint.Leijen.PrettyPrec
import Text.PrettyPrint.Leijen.DocExpr

import TypeUnary.Vec

import Shady.Misc (FMod(..),R)
import Data.Proof.EQ
-- import Shady.Language.Equality
-- import Shady.MechanicsGL (GlTexture)


{--------------------------------------------------------------------
    Type values
--------------------------------------------------------------------}

-- Primitive types
data ScalarT :: * -> * where
  Bool  :: ScalarT Bool
  Int   :: ScalarT Int
  Float :: ScalarT Float

instance Show (ScalarT a) where
  show Bool  = "bool"
  show Int   = "int"
  show Float = "float"

instance HasExprU ScalarT where
  exprU Bool  = var "bool"
  exprU Int   = var "int"
  exprU Float = var "float"

instance Pretty (ScalarT a) where pretty = text . show

vshow :: Show a => a -> Expr
vshow = var . show

instance HasExpr (ScalarT a) where expr = vshow

data VectorT n a = VectorT (Nat n) (ScalarT a)

instance Show (VectorT n a) where
  show (VectorT n t) = showVectorN (natToZ n) t

-- instance HasExpr a => HasExpr (VectorT a) where expr = expr1
-- instance HasExpr1 VectorT    where expr1 = var . show

instance HasExprU (VectorT n)   where exprU = expr
instance HasExpr  (VectorT n a) where expr  = var . show

showVectorN :: Integer -> ScalarT a -> String
showVectorN 1 p = show p
showVectorN n p = pref p ++ "vec" ++ show n
  where
    pref :: ScalarT b -> String
    pref Bool  = "b"
    pref Int   = "i"
    pref Float = ""

instance Pretty (VectorT n a) where pretty = text . show

-- | Encoding of texture ids in values.  I'm using 'Int' instead of
-- @GLuint@ here to avoid depending on OpenGL in this module & package.
type TextureId = Int

-- | An @n@-dimensional GLSL sampler.
data Sampler n =
  Sampler { samplerDim :: Nat n, samplerTexture :: TextureId }

type Sampler1 = Sampler N1
type Sampler2 = Sampler N2
type Sampler3 = Sampler N3

instance Show (Sampler n) where
  show (Sampler n tex) = "<Sampler "++show n++" "++show tex++">"

instance Pretty (Sampler n) where
  pretty = text . show

sampler1 :: TextureId -> Sampler1
sampler1 = Sampler one                  -- or Sampler nat

sampler2 :: TextureId -> Sampler2
sampler2 = Sampler two                  -- or Sampler nat

sampler3 :: TextureId -> Sampler3
sampler3 = Sampler three                -- or Sampler nat

-- | Extended types.  Vector types, samplers, unit, pairing, and functions.
data Type :: * -> * where
  VecT     :: (IsNat n, IsScalar a {-, Storable (Vec n a) -}) =>
              VectorT n a -> Type (Vec n a)
  SamplerT :: IsNat n => Nat n -> Type (Sampler n)
  UnitT    :: Type ()
  (:*:)    :: (HasType a, HasType b {-, Show a, Show b -}) =>
              Type a -> Type b -> Type (a ,  b)
  (:->:)   :: (HasType a, HasType b {-, Show a, Show b -}) =>
              Type a -> Type b -> Type (a -> b)

instance HasExpr (Type t) where
  expr (VecT     t) = expr t
  expr (SamplerT n) = var $ "sampler" ++ show n ++ "D"
  expr UnitT        = var "()"
  expr (a :*:  b)   = op InfixR 1 ":*" (expr a) (expr b)
  expr (a :->: b)   = op InfixR 0 "->" (expr a) (expr b)

instance HasExprU Type where exprU = expr

instance PrettyPrec (Type t) where prettyPrec = prettyExpr
instance Pretty     (Type t) where pretty     = prettyPrec 0
instance Show       (Type t) where show       = show . expr


{--------------------------------------------------------------------
    Generating type values
--------------------------------------------------------------------}

-- EXPERIMENTAL: Typeable constraints

-- | Has scalar type
class (Storable a, Typeable a, Show a) => IsScalar a where scalarT :: ScalarT a

-- The Storable and Show prereqs simplify explicit constraints at uses.

instance IsScalar Bool  where scalarT = Bool
instance IsScalar Int   where scalarT = Int
instance IsScalar Float where scalarT = Float


vectorT :: (IsNat n, IsScalar a) => VectorT n a
vectorT = VectorT nat scalarT

-- | Known types
class Show t => HasType t where typeT :: Type t

-- Sorry about that Show constraint.  It's ultimately motivated by
-- the constant folding optimization and from there creeps into *lots* of contexts.

-- The Show t is experimental.  If it works out, remove Show from a lot of contexts.

instance (IsNat n, IsScalar a {-, Storable (Vec n a)-}) =>
         HasType (Vec n a) where
   typeT = VecT vectorT

instance HasType () where typeT = UnitT
instance (HasType a, HasType b {-, Show a, Show b -}) =>
  HasType (a, b) where typeT = typeT :*: typeT
instance (HasType a, HasType b {-, Show a, Show b -}) =>
  HasType (a->b) where typeT = typeT :->: typeT

instance IsNat n => HasType (Sampler n) where
  typeT = SamplerT nat

-- | Reify a type
typeOf :: HasType a => a -> Type a
typeOf = const typeT

-- | Reify a type argument
typeOf1 :: HasType a => f a -> Type a
typeOf1 = const typeT

-- | Reify a type argument's argument
typeOf2 :: HasType a => g (f a) -> Type a
typeOf2 = const typeT


-- | Demonstration that a type argument is a vector type.
data IsVec :: * -> * where
  IsVec :: (IsNat n, IsScalar a) => IsVec (Vec n a)

-- | Check for a vector type
checkVec :: forall t. HasType t => Maybe (IsVec t)
checkVec =
  case (typeT :: Type t) of
    VecT _ -> Just IsVec
    _      -> Nothing

-- | Convenient wrapper around 'checkVec'.  Ignores argument.
checkVec' :: forall f t. HasType t => f t -> Maybe (IsVec t)
checkVec' = const checkVec



{--------------------------------------------------------------------
    Type equality
--------------------------------------------------------------------}

-- | Try to prove equality of primitive types
ptyEq :: ScalarT a -> ScalarT b -> Maybe (a :=: b)
ptyEq Bool  Bool  = Just Refl
ptyEq Int   Int   = Just Refl
ptyEq Float Float = Just Refl
ptyEq _     _     = Nothing

-- | Try to prove equality of types
vtyEq :: VectorT m a -> VectorT n b -> Maybe (Vec m a :=: Vec n b)
vtyEq (VectorT m a) (VectorT n b) = liftA2 liftEq2 (m `natEq` n) (a `ptyEq` b)

-- | Try to prove equality of types
tyEq :: Type c -> Type c' -> Maybe (c :=: c')
tyEq (VecT a)     (VecT  a')    = vtyEq a a'
tyEq (SamplerT n) (SamplerT n') = fmap liftEq (natEq n n')
tyEq UnitT        UnitT         = Just Refl
tyEq (a :*:  b)   (a' :*:  b')  = liftA2 liftEq2 (tyEq a a') (tyEq b b')
tyEq (a :->: b)   (a' :->: b')  = liftA2 liftEq2 (tyEq a a') (tyEq b b')
tyEq _            _             = Nothing

-- TODO: Maybe define a class & method for the various typed equality
-- functions, with a nice infix method name.

-- | Yields 'Just' 'Refl' if type-compatible /and/ equal.  Otherwise 'Nothing'.
(=:=) :: forall f a b. (HasType a, HasType b, SynEq f) =>
         f a -> f b -> Maybe (a :=: b)
fa =:= fb =
  case typeOf1 fa `tyEq` typeOf1 fb of
    Just Refl -> if fa =-= fb then Just Refl else Nothing
    Nothing   -> Nothing

-- | Same type and syntactically equal
(===) :: forall f a b. (HasType a, HasType b, SynEq f) =>
         f a -> f b -> Bool
fa === fb = isJust (fa =:= fb)

-- | Do two values have the same type.  If so, return a proof.
compatible :: (HasType a, HasType b) => a -> b -> Maybe (a :=: b)
x `compatible` y = typeOf x `tyEq` typeOf y

-- | Do two values have the same argument type.  If so, return a proof.
compatible1 :: (HasType a, HasType b) => f a -> g b -> Maybe (a :=: b)
x `compatible1` y = typeOf1 x `tyEq` typeOf1 y


{--------------------------------------------------------------------
    Convenient type synonyms
--------------------------------------------------------------------}

-- TODO: Maybe move R to Misc and use in defining EyePos in MechanicsGL

-- | Convenient short-hand
type R1 = Vec1 R
-- | Convenient short-hand
type R2 = Vec2 R
-- | Convenient short-hand
type R3 = Vec3 R
-- | Convenient short-hand
type R4 = Vec4 R

-- | Single boolean
type B1 = Vec1 Bool

-- | Unary predicate
type Pred1 a = a -> B1
-- | Binary predicate
type Pred2 a = a -> Pred1 a


{--------------------------------------------------------------------
    Notions of equality
--------------------------------------------------------------------}

infix 4 =-=, =--=

-- | Syntactic equality.  Requires same argument type.
class SynEq f where
  (=-=) :: HasType c => f c -> f c -> Bool

instance Eq x => SynEq (Const x) where (=-=) = (==)

-- | Higher-order variant of 'SynEq'.  Can be defined via '(=-=)', or vice versa.
class SynEq2 f where
  (=--=) :: (SynEq v, HasType c) => f v c -> f v c -> Bool


deriving instance Eq a => Eq (Const a b)


{--------------------------------------------------------------------
    Pairing
--------------------------------------------------------------------}

infixr 1 #, :#

class PairF f where
  (#) :: (HasType a, HasType b {-, Show a, Show b -}) =>
         f a -> f b -> f (a :# b)

-- | Syntactic alternative for pairing.  Convenient for right-associative
-- infix use.
type a :# b = (a,b)

class UnitF f where unit :: f ()


{--------------------------------------------------------------------
    Orphans
--------------------------------------------------------------------}

-- Pretty-printing here instead of Vec, so we can use VectorT.  Numeric
-- instances here because Show is here.

instance (IsNat n, IsScalar a, Pretty a) => Pretty (Vec n a) where
  pretty v | n == 1    = pretty (head as)
           | otherwise = pretty (vectorT :: VectorT n a) <> tupled (map pretty as)
    where as = toList v
          n  = length as

-- instance (IsNat n, IsScalar a, Show a) => Show (Vec n a) where
--   show v | n == 1    = show (head as)
--          | otherwise = show (vectorT :: VectorT n a)
--                        ++ "(" ++ intercalate "," (map show as) ++ ")"
--     where as = toList v
--           n  = length as

-- 2011-10-26: I removed the Show instance above in favor of a new
-- Haskell-eval'able Show instance in TypeUnary.Vec. To do: check whether
-- this change broke Shady's code generation. Maybe not, if the code
-- generation uses Pretty instead of Show.

instance (IsNat n, IsScalar a, Pretty a) => PrettyPrec (Vec n a)
instance (IsNat n, IsScalar a, Show   a) => HasExpr    (Vec n a)

-- Generate bogus Enum instance, needed by 'Integral'
#define INSTANCE_Enum

#define CONSTRAINTS IsNat n, IsScalar applicative_arg,
#define APPLICATIVE (Vec n)
#include "ApplicativeNumeric-inc.hs"


instance (IsNat n, IsScalar a, FMod a) => FMod (Vec n a) where
  fmod = liftA2 fmod
