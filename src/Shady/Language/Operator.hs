{-# LANGUAGE KindSignatures, GADTs, PatternGuards, TypeOperators
           , FlexibleContexts
  #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Language.Operator
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Known constants
----------------------------------------------------------------------

module Shady.Language.Operator
  ( Op(..), OpInfo(..), info
  , opExpr, opVal, opEq
  ) where

import Prelude hiding (all,any)

import Control.Applicative (liftA2)
import Data.Foldable (all,any,toList)

import Text.PrettyPrint.Leijen.DocExpr

import Control.Compose (result)

import Data.VectorSpace (VectorSpace(..),InnerSpace(..))

-- import Shady.Language.Equality
import Shady.Language.Type
-- import Shady.Vec
import Shady.Misc


{--------------------------------------------------------------------
    Operators
--------------------------------------------------------------------}

data Op   :: * -> * where
    -- Literal
  Lit     :: Show a => a -> Op a
    -- Booleans
  -- Hack: say that And/Or work on bool vectors.  Later, revert and
  -- implement the vector versions via the scalar versions.
  And     :: IsNat n => Op (Binop (Vec n Bool))
             -- Op (Binop B1)
  Or      :: IsNat n => Op (Binop (Vec n Bool))
             -- Op (Binop B1)
  Not     :: IsNat n => Op (Unop (Vec n Bool))
  EqualV  :: (IsNat n, IsScalar a, Eq a) =>
             Nat n -> Op (Vec n a -> Vec n a -> Vec n Bool)
  AllV    :: IsNat n => Op (Vec n Bool -> B1)
  AnyV    :: IsNat n => Op (Vec n Bool -> B1)
    -- Eq
  Equal   :: Eq (Vec n a) => Op (Pred2 (Vec n a))
    -- Ord
  Lt      :: (IsNat n, IsScalar a, Ord a) => Nat n -> Op (Vec n a -> Vec n a -> Vec n Bool)
  Le      :: (IsNat n, IsScalar a, Ord a) => Nat n -> Op (Vec n a -> Vec n a -> Vec n Bool)
  Min     :: (IsNat n, IsScalar a, Ord a) => Op (Binop (Vec n a))
  Max     :: (IsNat n, IsScalar a, Ord a) => Op (Binop (Vec n a))
    -- Num
  Negate  :: (IsNat n, IsScalar a, Num a) => Op (Unop  (Vec n a))
  Add     :: (IsNat n, IsScalar a, Num a) => Op (Binop (Vec n a))
  Sub     :: (IsNat n, IsScalar a, Num a) => Op (Binop (Vec n a))
  Mul     :: (IsNat n, IsScalar a, Num a) => Op (Binop (Vec n a))
  Abs     :: (IsNat n, IsScalar a, Num a) => Op (Unop  (Vec n a))
  Signum  :: (IsNat n, IsScalar a, Num a) => Op (Unop  (Vec n a))
    -- Integral
  Quot     :: (IsNat n, IsScalar a, Integral a) => Op (Binop (Vec n a))
  Rem      :: (IsNat n, IsScalar a, Integral a) => Op (Binop (Vec n a))
  Div      :: (IsNat n, IsScalar a, Integral a) => Op (Binop (Vec n a))
  Mod      :: (IsNat n, IsScalar a, Integral a) => Op (Binop (Vec n a))
    -- Fractional
  Recip    :: (IsNat n, IsScalar a, Fractional a) => Op (Unop  (Vec n a))
  Divide   :: (IsNat n, IsScalar a, Fractional a) => Op (Binop (Vec n a))
    -- Floating
  Sqrt     :: (IsNat n, IsScalar a, Floating a) => Op (Unop (Vec n a))
  Exp      :: (IsNat n, IsScalar a, Floating a) => Op (Unop (Vec n a))
  Log      :: (IsNat n, IsScalar a, Floating a) => Op (Unop (Vec n a))
  Sin      :: (IsNat n, IsScalar a, Floating a) => Op (Unop (Vec n a))
  Cos      :: (IsNat n, IsScalar a, Floating a) => Op (Unop (Vec n a))
  Asin     :: (IsNat n, IsScalar a, Floating a) => Op (Unop (Vec n a))
  Atan     :: (IsNat n, IsScalar a, Floating a) => Op (Unop (Vec n a))
  Acos     :: (IsNat n, IsScalar a, Floating a) => Op (Unop (Vec n a))
  Sinh     :: (IsNat n, IsScalar a, Floating a) => Op (Unop (Vec n a))
  Cosh     :: (IsNat n, IsScalar a, Floating a) => Op (Unop (Vec n a))
  Asinh    :: (IsNat n, IsScalar a, Floating a) => Op (Unop (Vec n a))
  Atanh    :: (IsNat n, IsScalar a, Floating a) => Op (Unop (Vec n a))
  Acosh    :: (IsNat n, IsScalar a, Floating a) => Op (Unop (Vec n a))
    -- RealFrac
  Truncate :: IsNat n => Op (Unop (Vec n R))
  Round    :: IsNat n => Op (Unop (Vec n R))
  Ceiling  :: IsNat n => Op (Unop (Vec n R))
  Floor    :: IsNat n => Op (Unop (Vec n R))
  FMod     :: (IsNat n, IsScalar a, FMod a) => Op (Binop (Vec n a))
    -- Vector
  -- VVec1   :: IsScalar a => Op (Vec1 a                         -> Vec1 a)
  VVec2   :: IsScalar a => Op (Vec1 a -> Vec1 a                 -> Vec2   a)
  VVec3   :: IsScalar a => Op (Vec1 a -> Vec1 a -> Vec1 a         -> Vec3 a)
  VVec4   :: IsScalar a => Op (Vec1 a -> Vec1 a -> Vec1 a -> Vec1 a -> Vec4  a)
  Dot     :: IsNat n => Op (Vec n R -> Vec n R -> R1)
  Swizzle :: (IsNat n, IsNat m, IsScalar a) =>
             Vec n (Index m) -> Op (Vec m a -> Vec n a)
    -- Nestable pairs
  Unit    :: Op ()
  Pair    :: Op (a -> b -> (a,b))
  Fst     :: Op ((a,b) -> a)
  Snd     :: Op ((a,b) -> b)
    -- Misc
  If       :: HasType a => Op (B1 -> Binop a)
  Cat      :: (IsNat m, IsNat n, IsNat (m :+: n), IsScalar a) =>
              Nat m -> Nat n -> VectorT (m :+: n) a
           -> Op (Vec m a -> Vec n a -> Vec (m :+: n) a)
  UniformV :: IsNat n => VectorT n a -> Op (Vec1 a -> Vec n a)
  Scale    :: (IsNat n, Num a, IsScalar a) => Op (Vec1 a -> Unop (Vec n a))
    -- Misc graphics-specific
  Texture  :: IsNat n => Nat n -> Op (Sampler n -> Vec n R -> R4)

-- TODO: eliminate Scale?  unsure.
instance Show (Op t) where show = oiName . info


{--------------------------------------------------------------------
    Fixity/precedence info
--------------------------------------------------------------------}

type Fixity = Maybe (Associativity, Int)

infixA :: Associativity -> Int -> Fixity
infixA ass n = Just (ass, n)

nofix :: Fixity
nofix  = Nothing

infixL, infixR, infixN :: Int -> Fixity

infixL = infixA InfixL
infixR = infixA InfixR
infixN = infixA Infix

one1 :: (a -> b) -> a -> Vec1 b
one1 = result vec1

one2 :: (a -> b -> c) -> a -> b -> Vec1 c
one2 = result one1

-- in1 :: (a -> b) -> Vec1 a -> Vec1 b
-- in1 = un1 ~> vec1                       -- or fmap

-- in2 :: (a -> b -> c) -> Vec1 a -> Vec1 b -> Vec1 c
-- in2 = un1 ~> in1

-- in1, in2 subsumed by fmap & liftA2.


{--------------------------------------------------------------------
    Operator info
--------------------------------------------------------------------}

data OpInfo a = OpInfo { oiName :: String, oiVal :: a, oiFix :: Fixity }

info :: Op a -> OpInfo a

info (Lit a)  = OpInfo (show a)   a        nofix

info And      = OpInfo "(&&)"     (liftA2 (&&))  (infixR 3)
info Or       = OpInfo "(||)"     (liftA2 (||))  (infixR 2)
info Not      = OpInfo "not"      (fmap not)     nofix
info Equal    = OpInfo "(==)"     (one2 (==))    (infixN 4)

info (EqualV n) = condN "(==)" "equal"         (liftA2 (==)) (infixN 4) n

info AllV     = OpInfo "all"      all'     nofix
info AnyV     = OpInfo "any"      any'     nofix

info (Lt n) = condN "(<)"  "lessThan"      (liftA2 (<) ) (infixN 4) n
info (Le n) = condN "(<=)" "lessThanEqual" (liftA2 (<=)) (infixN 4) n
info Min    = OpInfo "min"      min      nofix
info Max    = OpInfo "max"      max      nofix

info Negate   = OpInfo "negate"   negate   nofix
info Add      = OpInfo "(+)"      (+)      (infixL 6)
info Sub      = OpInfo "(-)"      (-)      (infixL 6)
info Mul      = OpInfo "(*)"      (*)      (infixL 7)
info Abs      = OpInfo "abs"      abs      nofix
info Signum   = OpInfo "sign"     signum   nofix

info Quot     = OpInfo "quot"     quot     nofix
info Rem      = OpInfo "rem"      rem      nofix
info Div      = OpInfo "div"      div      nofix
info Mod      = OpInfo "mod"      mod      nofix

info Recip    = OpInfo "recip"    recip    nofix
info Divide   = OpInfo "(/)"      (/)      (infixL 7)
info FMod     = OpInfo "mod"      fmod     nofix

info Sqrt     = OpInfo "sqrt"     sqrt     nofix
info Exp      = OpInfo "exp"      exp      nofix
info Log      = OpInfo "log"      log      nofix
info Sin      = OpInfo "sin"      sin      nofix
info Cos      = OpInfo "cos"      cos      nofix
info Asin     = OpInfo "asin"     asin     nofix
info Atan     = OpInfo "atan"     atan     nofix
info Acos     = OpInfo "acos"     acos     nofix
info Sinh     = OpInfo "sinh"     sinh     nofix
info Cosh     = OpInfo "cosh"     cosh     nofix
info Asinh    = OpInfo "asinh"    asinh    nofix
info Atanh    = OpInfo "atanh"    atanh    nofix
info Acosh    = OpInfo "acosh"    acosh    nofix

info Truncate = OpInfo "truncate" (i2f . truncate) nofix
info Round    = OpInfo "round"    (i2f . round)    nofix
info Ceiling  = OpInfo "ceiling"  (i2f . ceiling)  nofix
info Floor    = OpInfo "floor"    (i2f . floor)    nofix

info VVec2 = OpInfo "vec2" vvec2 nofix
info VVec3 = OpInfo "vec3" vvec3 nofix
info VVec4 = OpInfo "vec4" vvec4 nofix
info Dot   = OpInfo "dot"  (<.>) nofix
-- info (Dot n)   = condN "(*)" "dot"  (<.>) (infixL 7) n

info (Swizzle ixs) = OpInfo (swizzleName ixs) (swizzle ixs) nofix 

info Unit     = OpInfo "()"       ()       nofix
info Pair     = OpInfo "(#)"      (,)      (infixR 1)
info Fst      = OpInfo "fst"      fst      nofix
info Snd      = OpInfo "snd"      snd      nofix

info If           = OpInfo "cond"     if'           nofix
info (Cat _ _  t) = OpInfo (show t)   (<+>)         nofix
info (UniformV t) = OpInfo (show t)   (pureV . un1) nofix
info Scale        = OpInfo "(*)"      (*^)          (infixR 7)

info (Texture n) = OpInfo ("texture" ++ show n ++ "D") texture nofix

opVal :: Op a -> a
opVal = oiVal . info

-- Will compile-time texture sampling happen?  If so, implement it.
texture :: IsNat n => Sampler n -> Vec n R -> R4
texture = error "texture: no constant fold"

i2f :: Vec n Int -> Vec n Float
i2f = fmap fromIntegral

-- opFix :: Op a -> Fixity
-- opFix = oiFix . info

-- Pick one info for n==1 and another for other n.  For instance,
-- "(==)" vs "equal".
condN :: String -> String -> a -> Fixity -> Nat n -> OpInfo a
condN name1 _ val fixity (Succ Zero) = OpInfo name1 val fixity
condN _ namen val _      _           = OpInfo namen val nofix

vvec2 :: Vec1 a -> Vec1 a -> Vec2 a
vvec2 a b = un1 a :< b

vvec3 :: Vec1 a -> Vec1 a -> Vec1 a -> Vec3 a
vvec3 a b c = un1 a :< vvec2 b c

vvec4 :: Vec1 a -> Vec1 a -> Vec1 a -> Vec1 a -> Vec4 a
vvec4 a b c d = un1 a :< vvec3 b c d

all', any' :: Vec n Bool -> B1
all' = vec1 . all id
any' = vec1 . any id

-- Part name
part :: Index m -> Char
part (Index _ m) = "xyzw" !! fromIntegral (natToZ m)

parts :: Vec n (Index m) -> String
parts = map part . toList

-- getName :: Index m -> String
-- getName ix = "GET" ++ [part ix]

swizzleName :: Vec n (Index m) -> String
swizzleName ixs = "GET" ++ parts ixs


{--------------------------------------------------------------------
    Pretty printing
--------------------------------------------------------------------}

-- | Operator application
opExpr :: Op z -> [Expr] -> Expr
opExpr Not  [e]    = fun "!" e
opExpr Negate [e]  = fun "-" e
opExpr If [c,t,e]  = ifExpr c t e
opExpr (Swizzle ixs) [e] = dotX (map part (toList ixs)) e
opExpr Recip [e]  = lift (1.0 :: Float) / e
opExpr (UniformV (VectorT (Succ Zero) _)) [e] = e
opExpr oper [x,y] | Just (ass,p) <- fixity
                  = op ass p (infixize name) x y
 where
   OpInfo name _ fixity = info oper
opExpr oper xs = ccall (oiName (info oper)) xs


-- Make a name infix-ready.  "(+)" --> "+", and "div" --> "`div`"
infixize :: String -> String
infixize ('(':cs) = init cs
infixize n = "`" ++ n ++ "`"

if' :: B1 -> Binop a
if' c t e = if un1 c then t else e

ifExpr :: Expr -> Expr -> Expr -> Expr
ifExpr c t e = op Infix 0 "?" c $
               op Infix 1 ":" t e

-- TODO: Better formatting for ?:  I'd like to align ":" with "?", and I
-- don't know how (elegantly).


{--------------------------------------------------------------------
    Operator equality
--------------------------------------------------------------------}

-- Operator equality, including differently typed operators.
opEq :: Op a -> Op b -> Bool

-- This implementation assumes that different operators look different.
oper `opEq` oper' = oiName (info oper) == oiName (info oper')

-- A polymorphism variant doesn't work:
-- 
--   opEq = (==) `on` (oiName . info)

instance SynEq Op where (=-=) = opEq
