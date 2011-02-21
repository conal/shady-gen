{-# LANGUAGE GADTs, RankNTypes, KindSignatures, TypeOperators
           , StandaloneDeriving, GeneralizedNewtypeDeriving
           , PatternGuards, ScopedTypeVariables
           , FlexibleContexts, FlexibleInstances
           , TypeFamilies, TypeSynonymInstances
           , MultiParamTypeClasses, UndecidableInstances
           , EmptyDataDecls, CPP
  #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Language.Exp
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Expressions.
----------------------------------------------------------------------

module Shady.Language.Exp
  (
  -- * Variables
    Id, V(..), var, genVar
  -- * Patterns
  , Pat, patT, pat
  -- * Type paths
  , TPath, emptyP, fstP, sndP, namePath
  -- * Expressions
  , E(..), (:=>), (:=>*)
  -- , Es, exps, (:>-), (:>-*)
  -- * n-ary operator application
  , op1, op2, op3, op4
  -- * Optimizing expression-builders
  , pureE, fmapE, liftE2, liftE3, liftE4
  -- * Operations
  , notE
--   , true, false
--   , (&&*), (||*)
--   , (<*), (<=*), (>=*), (>*)
--   , (==*), (/=*)
  , (==^), (/=^)
  , truncateE, roundE, ceilingE, floorE -- , fmod, fmodE, fracE
  , allV, anyV
  , SamplerE, texture
  , lit
  , BoolE, FloatE, R1E, R2E, R3E, R4E, VecE
  , vec2, vec3, vec4
  , un2, un3, un4
  , getX, getY, getZ, getW, get, (<+>)
  , unitE, pairE, fstE, sndE, unPairE, uniform, uniformV
  , ComplexE
  -- * Conversion to expressions
  , ToE(..), toE, FromE(..), toFromE, patE -- , ToEs, EsT, exps
  , module Shady.Language.Type
  -- , module Shady.Cat
  -- * Temporary
  , letE
  )
  where

import Data.Monoid (Monoid(..),First(..))
import Data.Maybe (fromMaybe)
import Control.Applicative (Applicative(pure),(<$>))
import Control.Monad (liftM2)
import Control.Arrow ((&&&),second)

import Text.PrettyPrint.Leijen hiding ((<$>),(<+>))
import Text.PrettyPrint.Leijen.PrettyPrec
import Text.PrettyPrint.Leijen.DocExpr hiding (var,apply)
import qualified Text.PrettyPrint.Leijen.DocExpr as X

import Control.Compose (result,(~>))

import Data.Boolean

import Data.VectorSpace

import Data.NameM
import Shady.Language.Type hiding ((<+>),vec2,vec3,vec4,un2,un3,un4,get)
import Shady.Language.Glom
import qualified Shady.Vec as V
import Shady.Language.Operator
import Shady.Misc
import Shady.Complex


{--------------------------------------------------------------------
    Strays
--------------------------------------------------------------------}

deriving instance Functor     First
deriving instance Applicative First
deriving instance Monad       First

fromFirst :: a -> First a -> a
fromFirst a = fromMaybe a . getFirst


{--------------------------------------------------------------------
    Variables
--------------------------------------------------------------------}

-- | Variable name
type Id = String

-- | Typed variables
data V a = V { varName :: Id, varType :: Type a } deriving Show

instance SynEq V where V a _ =-= V b _ = a == b

-- TODO: consider replacing the VectorT a with a constraint: IsVector a =>

-- instance Show (V a) where show = varName

instance HasExprU V where
  exprU = X.var . show
  -- exprU (V i ty) = op InfixL 0 "::" (X.var ('x':show i)) (exprU ty)

instance HasExpr a => HasExpr (V a) where expr = exprU

-- Or:
-- 
-- instance HasExpr a => HasExpr (V a) where
--   expr (V name ty) = op InfixL 0 "::" (X.var name) (expr ty)
-- 
-- instance HasExprU V where exprU = expr

instance HasExpr a => PrettyPrec (V a) where prettyPrec = prettyExpr
instance HasExpr a => Pretty     (V a) where pretty     = prettyPrec 0

-- instance HasExpr a => Show (V a)       where showsPrec  = showsPrettyPrec

-- -- | Equality on variables, ignoring type.
-- vEq :: V a -> V b -> Bool
-- V n _ `vEq` V n' _ = n == n'

-- | Make a variable, inferring the type from context.
var :: HasType a => Id -> V a
var = flip V typeT

-- TODO: maybe split var into uvar and avar

genVar :: HasType a => NameM (V a)
genVar = var <$> genName


{--------------------------------------------------------------------
    Type paths
--------------------------------------------------------------------}

-- | Type path
newtype TPath = TPath String

-- | Empty type path
emptyP :: TPath
emptyP = TPath ""

-- | Extend a type path
fstP, sndP :: TPath -> TPath
fstP (TPath p) = TPath ('F' : p)
sndP (TPath p) = TPath ('S' : p)

-- | Augment a variable name with a type path
namePath :: String -> TPath -> String
namePath vname (TPath "") = vname
namePath vname (TPath p)  = vname ++ "_" ++ reverse p

-- TODO: use a safer separator than "_" (avoiding real variable names).
-- With "__", I get "OpenGL reserves names containing '__'"


{--------------------------------------------------------------------
    Patterns
--------------------------------------------------------------------}

-- | Variable patterns
type Pat = Glom V

-- | The type of a pattern
patT :: Pat a -> Type a
patT (BaseG (V _ t)) = t
patT UnitG           = UnitT
patT (a :* b)        = patT a :*: patT b


-- | Make a variable pattern, inferring the type from context.
pat :: HasType a => String -> Pat a
pat vname = divvy emptyP typeT
 where
   divvy :: TPath -> Type s -> Pat s
   divvy _    UnitT      = UnitG
   divvy path (a :*:  b) = divvy (fstP path) a :* divvy (sndP path) b
   divvy _    (_ :->: _) = error "pat: function type not handled"
   divvy path t          = BaseG (V (namePath vname path) t)

-- Note divvy is not quite a fmapU, because of the path accumulation.
-- Look out for similar definitions.


{--------------------------------------------------------------------
    Simple expressions
--------------------------------------------------------------------}

infixl 9 :^

-- | Simple expressions (no 'Let').  Statically typed 
data E :: * -> * where
  Op   :: Op a -> E a                   -- ^ operator/constant
  Var  :: V  a -> E a                   -- ^ variable
  (:^) :: HasType a =>
          E (a -> b) -> E a -> E b      -- ^ application
  Lam  :: HasType a =>
          V a -> E b -> E (a -> b)      -- ^ abstraction

instance SynEq E where
  Op o   =-= Op o'   = o =-= o'
  Var v  =-= Var v'  = v =-= v'
  f :^ x =-= g :^ y  = f === g && x === y
  _      =-= _       = False

-- TODO: what about lambdas?  False negatives are okay for our use, which
-- is optimization.

-- | Short-hand for beta-redex
letE :: (HasType a, HasType b) =>
        V a -> E a -> E b -> E b
letE v a b = Lam v b :^ a


instance HasExpr (E a) where
  expr (Op oper)       = X.var (show oper)
  expr (Var (V n _))   = X.var n
  expr e@(_ :^ _)      = appExpr e []
  expr (Lam (V n _) f) = lambdaX n (expr f)


-- Application expr, passing in argument exprs.
appExpr :: forall a. E a -> [Expr] -> Expr
appExpr (Op o)                             xs = opExpr o xs
appExpr (Op Not :^ (Op (Lt  n) :^ a :^ b)) xs = appExpr (Op (Le n) :^ b :^ a) xs
appExpr (Op Mul :^ a :^ (Op Recip :^ b))   xs = appExpr (Op Divide :^ a :^ b) xs
appExpr (Op Add :^ a :^ (Op Negate :^ b))  xs = appExpr (Op Sub :^ a :^ b) xs
appExpr e@(Op (Cat _ _ _) :^ _ :^ _)       xs
  | First (Just e') <- catFix e               = appExpr e' xs
appExpr (Op (Swizzle ixs) :^ v)            xs 
  | Just e' <- swizzleOpt ixs v               = appExpr e' xs
appExpr (Lam v b :^ a)                     xs = foldl ($$) (letExpr v a b) xs
appExpr (f :^ e)                           xs = appExpr f (expr e : xs)
appExpr f                                  xs = foldl ($$) (expr f) xs


-- Flatten stacked cats.
catFix :: a :=>? a
catFix (Op (Cat (Succ Zero) (Succ Zero) _) :^ a :^ b) =
  pure (Op VVec2 :^ a :^ b)
catFix (Op (Cat (Succ Zero) _ _) :^ a :^ b) = catFix b >>= consV a
catFix _ = mempty

consV :: One a :=> Vec n a :=>? (Vec (S n) a)
consV a (Op VVec2 :^ b :^ c)      = pure (Op VVec3 :^ a :^ b :^ c)
consV a (Op VVec3 :^ b :^ c :^ d) = pure (Op VVec4 :^ a :^ b :^ c :^ d)
consV _ _                         = mempty

-- e.g., foo.xyz --> foo if foo is 3D
swizzleOpt :: forall n m a. (IsNat m, IsNat n) =>
              Vec n (Index m) -> E (Vec m a) -> Maybe (E (Vec n a))
swizzleOpt ixs v | Just Refl <- m `natEq` n, ixs == indices n = Just v
                 | otherwise                                  = Nothing
 where
   m = nat :: Nat m
   n = nat :: Nat n



-- Let expression
letExpr :: HasType a => V a -> E a -> E b -> Expr
letExpr (V n _) a b = letX n (expr a) (expr b)

-- exprFun :: (HasExpr c, HasType a) =>
--            (E a -> c) -> Id -> Expr
-- exprFun f = expr . f . Var . var . idName

instance PrettyPrec (E a) where prettyPrec = prettyExpr
instance Pretty     (E a) where pretty     = prettyPrec 0
instance Show       (E a) where show       = show . pretty


infixr 7 :=>, :=>*

-- | Function from expressions.  Nestable.
type a :=> b = E a -> b

-- | Expression to expression.  Ends a chain of '(:=>)'
type a :=>* b = a :=> E b


infixr 7 :=>?
-- | Expression to possible expression.  Ends a chain of '(:=>)'.
type a :=>? b = a :=> First (E b)


{--------------------------------------------------------------------
    Convenient n-ary operator application
--------------------------------------------------------------------}

-- | Convenient operator application
op1 :: (HasType a, HasType b) =>
       Op (a -> b) -> a :=>* b
op1 o a = Op o :^ a

-- | Convenient operator application
op2 :: (HasType a, HasType b, HasType c) =>
       Op (a -> b -> c) -> a :=> b :=>* c
op2 o a b = op1 o a :^ b

-- | Convenient operator application
op3 :: (HasType a, HasType b, HasType c, HasType d) =>
       Op (a -> b -> c -> d) -> a :=> b :=> c :=>* d
op3 o a b c = op2 o a b :^ c

-- | Convenient operator application
op4 :: (HasType a, HasType b, HasType c, HasType d, HasType e) =>
       Op (a -> b -> c -> d -> e) -> a :=> b :=> c :=> d :=>* e
op4 o a b c d = op3 o a b c :^ d


{--------------------------------------------------------------------
    Simplification / optimization
--------------------------------------------------------------------}

infix 0 @>
-- | Simplification result with fall-back value.
(@>) :: First a -> a -> a
(@>) = flip fromFirst



-- | Left identity: @i `op` a == a@
identityL :: Eq a => a -> a :=> b :=>? b
identityL i (Op (Lit u)) b | u == i = pure b
identityL _ _ _                     = mempty

-- | Right identity: @a `op` i == a@
identityR :: Eq b => b -> a :=> b :=>? a
identityR i a (Op (Lit v)) | v == i = pure a
identityR _ _ _                     = mempty

-- | Symmetric identity, combining 'identityL' and 'identityR'.
identity :: Eq a => a -> a :=> a :=>? a
identity = identityL `mappend` identityR

-- Will GHC optimize 'identity' to the following?

-- identity i (Op (Lit u)) b | u == i = pure b
-- identity i a (Op (Lit v)) | v == i = pure a
-- identity _ _ _                     = mempty

-- | Annihilator: @z * a == z@
annihilator :: Eq a => a -> a :=> a :=>? a
annihilator z (Op (Lit u)) _ | u == z = pure (pureE z)
annihilator z _ (Op (Lit v)) | v == z = pure (pureE z)
annihilator _ _ _                     = mempty


-- | Inverse-related properties
inverse :: Op (a -> a -> a) -> a :=> a :=>? a
inverse Add a (Op Negate :^ b) | a =-= b = pure 0
inverse Add (Op Negate :^ b) a | a =-= b = pure 0
inverse Mul a (Op Recip  :^ b) | a =-= b = pure 1
inverse Mul (Op Recip  :^ b) a | a =-= b = pure 1
inverse Mul a (Op (Lit (-1)))            = pure (negate a)
inverse Mul (Op (Lit (-1))) a            = pure (negate a)
inverse _   _ _                          = mempty


-- | Commute, to get literals together: @3 + a == a + 3@,
-- @(a + 3) + b == (a + b) + 3@.
-- 
-- Might be a bad idea, as it can break sharing.  Think through.
-- Not really effective without associate, which breaks even more sharing.
commute :: a :=> a :=>? a
-- commute a@(Op (Lit _)) b                  = pure (b + a)
-- commute (Op Add :^ a :^ b@(Op (Lit _))) c = pure ((a + c) + b)
commute _ _                               = mempty


#define SIMPLIFY

-- | Operator-specific simplifation (unary)
simple1 :: Op (a -> b) -> a :=>? b
#ifdef SIMPLIFY
simple1 Negate (Op Negate :^ a)    = pure a
simple1 Negate (Op Mul :^ a :^ b)  = pure (negate a * b) -- *
simple1 Negate (Op Add :^ a :^ b)  = pure (negate a + negate b) -- *
simple1 Recip  (Op Recip  :^ a)    = pure a
simple1 Fst    (Op Pair :^ a :^ _) = pure a
simple1 Snd    (Op Pair :^ _ :^ b) = pure b
simple1 Cos    (Op Negate :^ a)    = pure (cos a)
simple1 Sin    (Op Negate :^ a)    = pure (- sin a)

-- * Pushing the negate inward increases opportunities for vectorization,
-- but can break sharing.  For Add, it also increases cost a bit.

-- TODO: more
#endif
simple1 _ _ = mempty

-- | Operator-specific simplifation (binary)
simple2 :: Op (a -> b -> c) -> a :=> b :=>? c
#ifdef SIMPLIFY
simple2 Add         = identity    0 `mappend` addMul `mappend`
                      inverse Add   `mappend` commute
simple2 Mul         = annihilator 0 `mappend` identity 1 `mappend`
                      inverse Mul   `mappend` commute `mappend` mulNegNeg
simple2 (Cat _ _ _) = (<+?>)
#endif
simple2 _           = mempty

-- TODO: Change identity and annihilator to take Add and Mul as
-- arguments.  Then refactor for more reuse between Add & Mul and perhaps
-- all binary ops well.

-- simple2 Pair = pairFstSnd

-- | Operator-specific simplifation (ternary)
simple3 :: Op (a -> b -> c -> d) -> a :=> b :=> c :=>? d
#ifdef SIMPLIFY
simple3 If (Op (Lit c)) a b = pure $ if un1 c then a else b
simple3 If _ a b | a =-= b  = pure a
-- TODO: more
#endif
simple3 _ _ _ _ = mempty


-- | Operator-specific simplifation (quaternary)
simple4 :: Op (a -> b -> c -> d -> e) -> a :=> b :=> c :=> d :=>? e

#ifdef SIMPLIFY
-- TODO: more
#endif
simple4 _ = mempty
{-
-}


-- Vectorization

infix 1 <+?>
(<+?>) :: forall n m a.
          (IsNat n, IsNat m, IsScalar a,
           IsNat (m :+: n), Show a) =>
          Vec m a :=> Vec n a :=>? Vec (m :+: n) a

-- Comment out the first rule as a temp work-around for glsl 1.2

-- a <+> a = a.(all<+>all)
a <+?> b | n' > 1  -- for glsl 1.2, which doesn't allow swizzling scalars.
         , Just Refl <- a =:= b = pure (Op (Swizzle (is V.<+> is)) :^ a)
 where
   -- With -XNoMonomorphismRestriction, we get an Ambiguous type variable.
   -- If I then add ":: n", ghc doesn't terminate.
   n :: Nat n
   n  = nat
   n' = natToZ n
   is = indices n

-- a <+> a.js = a.(all<+>js)
a <+?> Op (Swizzle js) :^ b | Just Refl <- a =:= b =
  pure (Op (Swizzle (indices nat V.<+> js)) :^ a)
-- a.is <+> a = a.(is<+>all)
Op (Swizzle is) :^ a <+?> b | Just Refl <- a =:= b =
  pure (Op (Swizzle (is V.<+> indices nat)) :^ a)
-- a.is <+> a.js = a.(is<+>js)
Op (Swizzle is) :^ a <+?> Op (Swizzle js) :^ b
  | Just Refl <- a =:= b
  = pure (Op (Swizzle (is V.<+> js)) :^ a)

Op Min  :^ a :^ a' <+?> Op Min  :^ b :^ b' = pure ((a <+> b) `min`   (a' <+> b'))
Op Max  :^ a :^ a' <+?> Op Max  :^ b :^ b' = pure ((a <+> b) `max`  (a' <+> b'))
Op Add  :^ a :^ a' <+?> Op Add  :^ b :^ b' = pure ((a <+> b) +      (a' <+> b'))
Op Sub  :^ a :^ a' <+?> Op Sub  :^ b :^ b' = pure ((a <+> b) -      (a' <+> b'))
Op Mul  :^ a :^ a' <+?> Op Mul  :^ b :^ b' = pure ((a <+> b) *      (a' <+> b'))
Op Quot :^ a :^ a' <+?> Op Quot :^ b :^ b' = pure ((a <+> b) `quot` (a' <+> b'))
Op Rem  :^ a :^ a' <+?> Op Rem  :^ b :^ b' = pure ((a <+> b) `rem`  (a' <+> b'))
Op Div  :^ a :^ a' <+?> Op Div  :^ b :^ b' = pure ((a <+> b) `div`  (a' <+> b'))
Op Mod  :^ a :^ a' <+?> Op Mod  :^ b :^ b' = pure ((a <+> b) `mod`  (a' <+> b'))
Op FMod :^ a :^ a' <+?> Op FMod :^ b :^ b' = pure ((a <+> b) `fmod` (a' <+> b'))

Op Divide :^ a :^ a' <+?> Op Divide :^ b :^ b' = pure ((a <+> b) / (a' <+> b'))

Op Negate   :^ a <+?> Op Negate   :^ b = pure (negate    (a <+> b))
Op Recip    :^ a <+?> Op Recip    :^ b = pure (recip     (a <+> b))
Op Abs      :^ a <+?> Op Abs      :^ b = pure (abs       (a <+> b))
Op Signum   :^ a <+?> Op Signum   :^ b = pure (signum    (a <+> b))
Op Sqrt     :^ a <+?> Op Sqrt     :^ b = pure (sqrt      (a <+> b))
Op Exp      :^ a <+?> Op Exp      :^ b = pure (exp       (a <+> b))
Op Log      :^ a <+?> Op Log      :^ b = pure (log       (a <+> b))
Op Sin      :^ a <+?> Op Sin      :^ b = pure (sin       (a <+> b))
Op Cos      :^ a <+?> Op Cos      :^ b = pure (cos       (a <+> b))
Op Asin     :^ a <+?> Op Asin     :^ b = pure (asin      (a <+> b))
Op Acos     :^ a <+?> Op Acos     :^ b = pure (acos      (a <+> b))
Op Sinh     :^ a <+?> Op Sinh     :^ b = pure (sinh      (a <+> b))
Op Asinh    :^ a <+?> Op Asinh    :^ b = pure (asinh     (a <+> b))
Op Atanh    :^ a <+?> Op Atanh    :^ b = pure (atanh     (a <+> b))
Op Acosh    :^ a <+?> Op Acosh    :^ b = pure (acosh     (a <+> b))
Op Truncate :^ a <+?> Op Truncate :^ b = pure (truncateE (a <+> b))
Op Round    :^ a <+?> Op Round    :^ b = pure (roundE    (a <+> b))
Op Ceiling  :^ a <+?> Op Ceiling  :^ b = pure (ceilingE  (a <+> b))
Op Floor    :^ a <+?> Op Floor    :^ b = pure (floorE    (a <+> b))
Op Not      :^ a <+?> Op Not      :^ b = pure (notE      (a <+> b))

-- I'm using @^ on the RHSs in order to get CSE.  If I used the smart
-- constructors (min etc), I'd get more operator-specific optimization.
-- For now, I assume there won't be any.  If I'm wrong, revisit.

-- The next three are trickier, because the result scalar type (Bool) does
-- not determine the argument types, which could thus differ.  Hence the
-- compatibility check.

Op (EqualV _) :^ a :^ a' <+?> Op (EqualV _) :^ b :^ b'
  | Just Refl <- a `compatible1` b
  = pure ((a <+> b) ==* (a' <+> b'))

Op (Lt _) :^ a :^ a' <+?> Op (Lt _) :^ b :^ b'
  | Just Refl <- a `compatible1` b
  = pure ((a <+> b) <* (a' <+> b'))

Op (Le _) :^ a :^ a' <+?> Op (Le _) :^ b :^ b'
  | Just Refl <- a `compatible1` b
  = pure ((a <+> b) <=* (a' <+> b'))

_ <+?> _ = mempty

-- TODO: Eliminate the nat arguments to EqualV etc if they're now unused

-- | Undistribute: @a*b + a*b' == a*(b+b')@.  Also, dot products
-- @a*b + a'*b' == (a,a') <.> (b,b')@
addMul :: forall n a.
          (IsNat n, IsScalar a, Num a) =>
          Vec n a :=> Vec n a :=>? Vec n a
-- (Op Mul :^ a :^ b) `addMul` (Op Mul :^ a' :^ b')
--   | a =-= a' = pure (a * (b + b'))
--   | b =-= b' = pure ((a + a') * b)

(Op Mul :^ a :^ b) `addMul` (Op Mul :^ a' :^ b')
  | Just Refl <- (typeT :: Type (Vec n a)) `tyEq` (typeT :: Type R1)
  = pure $ (a <+> a') <.> (b <+> b')

(Op Dot :^ a :^ b) `addMul` (Op Mul :^ a' :^ b')
  | Just Refl      <- (typeT :: Type (Vec n a)) `tyEq` (typeT :: Type R1)
  , Just CanExtend <- canExtendE a
  = pure $ (a <+> a') <.> (b <+> b')

_ `addMul` _ = mempty

-- Proof that an n-vector can be extened by one element
data CanExtend :: * -> * where
  CanExtend :: IsNat (n :+: OneT) => CanExtend n

canExtend :: forall n. IsNat n => Maybe (CanExtend n)
canExtend =
  case (nat :: Nat n) of
    Zero                    -> j
    Succ Zero               -> j
    Succ (Succ Zero)        -> j
    Succ (Succ (Succ Zero)) -> j
    _                       -> Nothing
 where
   j :: IsNat (m :+: OneT) => Maybe (CanExtend m)
   j = Just CanExtend

-- Pull in the type parameter
canExtendE :: IsNat n => f (Vec n a) -> Maybe (CanExtend n)
canExtendE = const canExtend


-- -a * -b == a * b
mulNegNeg :: (IsNat n, IsScalar a, Num a) =>
             Vec n a :=> Vec n a :=>? Vec n a
mulNegNeg (Op Negate :^ a) (Op Negate :^ b) = pure (a * b)
mulNegNeg _ _ = mempty



-- I don't know how to get the following simplification to type-check:

-- -- Surjectivity of pairs: (fst c, snd c) == c
-- pairFstSnd :: -- forall a b. (HasType a, HasType b) =>
--           a :=> b :=>? (a,b)
-- pairFstSnd (Op Fst :^ c) (Op Snd :^ c')
--   | Just Refl <- tyEq (typeT :: c) (typeT :: c'), 
--     c =-= c'  = pure c
-- surjectivePair _ _ = mempty

{-
-- -a * b == - (a * b) ; a * -b == - (a * b)
mulNegUp, negMul :: (IsNat n, IsScalar a, Num a) =>
                    (Vec n a) :=> (Vec n a) :=>? (Vec n a)
mulNegUp (Op Negate :^ a) b = negMul a b
mulNegUp a (Op Negate :^ b) = negMul a b
mulNegUp _ _                = mempty

negMul a b = pure (Op Negate :^ (Op Mul :^ a :^ b))
-}


{--------------------------------------------------------------------
    'E' Lifters
--------------------------------------------------------------------}

-- Basic lifters.  I've named them suggestively of 'Functor' and
-- 'Applicative' methods.  They fit generalized versions of these classes
-- with the arrows being operators.

-- | Literal expression
pureE :: Show a => a -> E a
pureE = Op . Lit

-- | Apply a unary operator, with constant-folding and simplifications
fmapE :: (HasType a, HasType b {-, Show b-}) =>
         Op (a -> b) -> a :=>* b
#ifdef SIMPLIFY
fmapE o (Op (Lit x)) = Op (Lit (opVal o x))
#endif
fmapE o a = simple1 o a @> op1 o a

-- | Apply a binary operator, with constant-folding and simplifications
liftE2 :: (HasType a, HasType b, HasType c {-, Show c-}) =>
          Op (a -> b -> c) -> a :=> b :=>* c
#ifdef SIMPLIFY
liftE2 o (Op (Lit x)) (Op (Lit y)) = Op (Lit (opVal o x y))
#endif
liftE2 o a b = simple2 o a b @> op2 o a b

-- | Apply a ternary operator, with constant-folding and simplifications
liftE3 :: (HasType a, HasType b, HasType c, HasType d {-, Show d-}) =>
          Op (a -> b -> c -> d) -> a :=> b :=> c :=>* d
#ifdef SIMPLIFY
liftE3 o (Op (Lit x)) (Op (Lit y)) (Op (Lit z)) = Op (Lit (opVal o x y z))
#endif
liftE3 o a b c = simple3 o a b c @> op3 o a b c

-- | Apply an quaternary operator, with constant-folding and simplifications
liftE4 :: (HasType a, HasType b, HasType c, HasType d, HasType e {-, Show e-}) =>
          Op (a -> b -> c -> d -> e) -> a :=> b :=> c :=> d :=>* e
#ifdef SIMPLIFY
liftE4 o (Op (Lit w)) (Op (Lit x)) (Op (Lit y)) (Op (Lit z)) =
  Op (Lit (opVal o w x y z))
#endif
liftE4 o a b c d = simple4 o a b c d @> op4 o a b c d


{--------------------------------------------------------------------
    E Instances
--------------------------------------------------------------------}

-- The types of some methods prevent them from being lifted to expressions
noOv :: String -> a
noOv meth = error $ meth ++ ": No overloading for E"

instance Eq (E a) where
  (==) = noOv "(==)"
  (/=) = noOv "(/=)"

instance (IsNat n, IsScalar a, Ord a, Show a) => Ord (E (Vec n a)) where
  min = liftE2 Min
  max = liftE2 Max
  (<) = noOv "(<)"

instance IsNat n => Boolean (VecE n Bool) where
  false = pureU  False
  true  = pureU  True
  notB  = fmapE  Not
  (&&*) = liftE2 And
  (||*) = liftE2 Or

pureU :: (IsNat n, IsScalar a) => a -> VecE n a
pureU x = uniformV' (pureE (vec1 x))

-- Here's the weird deal: if pureU uses uniformV instead of uniformV',
-- then we trigger a bug in ghc 6.10.3:
-- 
--     ghc: panic! (the 'impossible' happened)
--       (GHC version 6.10.3 for i386-unknown-linux):
--             initC: srt_lbl
-- 
-- The definitions of uniformV and uniformV' are identical.  If I
-- change the definition of uniform to use uniformV' instead of
-- uniformV, then uniformV' becomes the fatal choice in pureU.

uniformV' :: (IsNat n, IsScalar a, Show a) =>
             One a :=>* Vec n a
uniformV' = fmapE (UniformV vectorT)

-- Does GLSL have conjunction and disjunction on boolean vectors?  If so,
-- then I can generalize this instance (using uniformV for false & true).
-- Even if GLSL doesn't have it, I could generate code.  Then I can keep
-- EqB and OrdB deriving from Boolean.

-- | Transitional synonym for notB
notE :: IsNat n => Vec n Bool :=>* Vec n Bool
notE = notB

-- TODO: Eliminate notE

instance (IsNat n, IsScalar a, Show a) => IfB BoolE (VecE n a) where
  ifB = liftE3 If

-- -- | Synonym for 'ifB' (transitional)
-- ifE :: IfB b a => b -> a -> a -> a
-- ifE = ifB

-- -- | Expression-lifted conditional with condition last
-- ifE' :: IfB b a => a -> a -> b -> a
-- ifE' = boolean


instance (IsNat n, IsScalar a, Eq a, Show a) => EqB (VecE n Bool) (VecE n a) where
  (==*) = liftE2 (EqualV nat)

instance (IsNat n, IsScalar a, Ord a, Show a) =>
         OrdB (VecE n Bool) (VecE n a) where
  (<*) = liftE2 (Lt nat)

infix  4  ==^, /=^

-- | Vector equality, resulting in a single Bool.  See also '(==*)'.
(==^) :: (IsNat n, IsScalar a, Eq a, Show a) =>
         Vec n a :=> Vec n a :=>* B1
(==^) = liftE2 Equal

-- | Vector inequality, resulting in a single Bool.   See also '(/=*)'.
(/=^) :: (IsNat n, IsScalar a, Eq a, Show a) =>
         Vec n a :=> Vec n a :=>* B1
(/=^) = (result.result) notE (==^)


instance Enum a => Enum (E a) where
  succ           = noOv "succ"
  pred           = noOv "pred"
  toEnum         = noOv "toEnum"
  fromEnum       = noOv "fromEnum"
  enumFrom       = noOv "enumFrom"
  enumFromThen   = noOv "enumFromThen"
  enumFromTo     = noOv "enumFromTo"
  enumFromThenTo = noOv "enumFromThenTo"

instance (IsNat n, IsScalar a, Num a) =>
         Num (E (Vec n a)) where
  fromInteger = pureE . fromInteger
  negate      = fmapE  Negate
  (+)         = liftE2 Add
  (*)         = liftE2 Mul
  abs         = fmapE  Abs
  signum      = fmapE  Signum

instance (IsNat n, IsScalar a, Ord a, Num a) =>
         Real (E (Vec n a)) where
  toRational = noOv "toRational"

instance (IsNat n, IsScalar b, Integral b) =>
         Integral (E (Vec n b)) where
  quot      = liftE2 Quot
  rem       = liftE2 Rem
  div       = liftE2 Div
  mod       = liftE2 Mod
  quotRem   = both quot rem
  divMod    = both div mod
  toInteger = noOv "toInteger"

both :: (a -> b -> c) -> (a -> b -> c') -> (a -> b -> (c,c'))
both f g a b = (f a b, g a b)

instance (IsNat n, IsScalar b, Fractional b) => Fractional (E (Vec n b)) where
  recip        = fmapE Recip
  fromRational = pureE . fromRational

instance (IsNat n, IsScalar b, Floating b) => Floating (E (Vec n b)) where
  pi    = pureE pi
  sqrt  = fmapE Sqrt
  exp   = fmapE Exp
  log   = fmapE Log
  sin   = fmapE Sin
  cos   = fmapE Cos
  asin  = fmapE Asin
  atan  = fmapE Atan
  acos  = fmapE Acos

  -- GLSL 1.2 doesn't support hyperbolic trig.  Substitute these
  -- definitions.  TODO: two paths, depending on GLSL version.

  sinh x           = (exp x - exp (-x)) / 2
  cosh x           = (exp x + exp (-x)) / 2
  asinh x          = log (x + sqrt (x*x + 1))
  acosh x          = log (x + sqrt (x*x - 1))
  atanh x          = (log (1 + x) - log (1 - x)) / 2

--   sinh  = fmapE Sinh
--   cosh  = fmapE Cosh
--   asinh = fmapE Asinh
--   atanh = fmapE Atanh
--   acosh = fmapE Acosh

instance (IsNat n, IsScalar b, RealFrac b) => RealFrac (E (Vec n b)) where
  properFraction = noOv "properFraction"
  truncate       = noOv "truncate"
  round          = noOv "round"
  ceiling        = noOv "ceiling"
  floor          = noOv "floor"

-- truncateE, roundE, ceilingE, floorE :: (RealFrac a, Integral b) => a :=>* b

-- Funky types, to match GLSL:
truncateE, roundE, ceilingE, floorE :: IsNat n => Vec n R :=>* Vec n R

truncateE = fmapE Truncate
roundE    = fmapE Round
ceilingE  = fmapE Ceiling
floorE    = fmapE Floor

instance (IsNat n, IsScalar a, FMod a) => FMod (E (Vec n a)) where
  fmod = liftE2 FMod

instance (IsNat n, IsScalar a, FMod a, RealFrac a) => Frac (E (Vec n a)) where
  frac = fracViaFmod


{--------------------------------------------------------------------
    Boolean vector operations
--------------------------------------------------------------------}

-- -- | Component-wise 'not'
-- notV :: (IsNat n) => (Vec n Bool) :=>* (Vec n Bool)
--                    -- Vec n Bool :=>* Vec n Bool
-- notV = fmapE NotV

-- | Are all of the 'Bool's true?
allV :: IsNat n => Vec n Bool :=>* B1
allV = fmapE AllV

-- | Is all of the 'Bool's true?
anyV :: IsNat n => Vec n Bool :=>* B1
anyV = fmapE AnyV



{--------------------------------------------------------------------
    Misc operations
--------------------------------------------------------------------}

type SamplerE n = E (Sampler n)

-- | Texturing
texture :: IsNat n => Sampler n :=> Vec n R :=>* R4
texture = liftE2 (Texture nat)

-- | Literal value
lit :: Show a => a -> E a
lit = Op . Lit


-- | 'Bool'
type BoolE = E B1

-- | 'Float' expression
type FloatE = E R1

type R1E = E R1
type R2E = E R2
type R3E = E R3
type R4E = E R4

-- | Expression vector
type VecE n a = E (Vec n a)


-- vec1 :: (IsScalar a, Show a) => a :=>* (Vec1 a)
-- vec1 = fmapE VVec1

vec2 :: (IsScalar a, Show a) => One a :=> One a                     :=>* Two a
vec3 :: (IsScalar a, Show a) => One a :=> One a :=> One a           :=>* Three a
vec4 :: (IsScalar a, Show a) => One a :=> One a :=> One a :=> One a :=>* Four a

vec2 a b     = a <+> b
vec3 a b c   = a <+> vec2 b c
vec4 a b c d = a <+> vec3 b c d

-- vec2 = liftE2 VVec2
-- vec3 = liftE3 VVec3
-- vec4 = liftE4 VVec4

un2 :: IsScalar a => Two a :=> (E (One a), E (One a))
un2 u = (getX u, getY u)

un3 :: IsScalar a => Three a :=> (E (One a), E (One a), E (One a))
un3 u = (getX u, getY u, getZ u)

un4 :: IsScalar a => Four a :=> (E (One a), E (One a), E (One a), E (One a))
un4 u = (getX u, getY u, getZ u, getW u)


-- | Extract X component
getX :: (IsNat n, IsScalar a, Show a) =>
        Vec (S n)             a :=>* One a
getX = get index0
-- | Extract Y component
getY :: (IsNat n, IsScalar a, Show a) =>
        Vec (S (S n))         a :=>* One a
getY = get index1
-- | Extract Z component
getZ :: (IsNat n, IsScalar a, Show a) =>
        Vec (S (S (S n)))     a :=>* One a
getZ = get index2
-- | Extract W component
getW :: (IsNat n, IsScalar a, Show a) =>
        Vec (S (S (S (S n)))) a :=>* One a
getW = get index3

-- | Extract vector component
get :: (IsNat n, IsScalar a, Show a) =>
       Index n -> (Vec n a) :=>* One a
get i = fmapE (Swizzle (vec1 i))


infixl 1 <+>
-- | Concatenation of vectors
(<+>) :: (IsNat m, IsNat n, IsNat (m :+: n), IsScalar a, Show a) =>
         Vec m a :=> Vec n a :=>* Vec (m :+: n) a
(<+>) = liftE2 (Cat nat nat vectorT)



-- | Expression-lifted '()'
unitE :: E ()
unitE = pureE ()


-- | Expression-lifted '(,)'
pairE :: (HasType a, HasType b{-, HasExpr a, HasExpr b-}) =>
         -- (Show a, Show b) =>
         E a -> E b -> E (a,b)
pairE = liftE2 Pair

-- | Expression-lifted 'fst'
fstE :: (HasType a, HasType b {-, Show b -} {-, HasExpr a, HasExpr b-}) =>
        Show a => E (a,b) -> E a
fstE = fmapE Fst

-- | Expression-lifted 'snd'
sndE :: (HasType a, HasType b {-, Show a-} {-, HasExpr a, HasExpr b-}) =>
        Show b => E (a,b) -> E b
sndE = fmapE Snd

-- | Unpack a pair
unPairE :: (HasType a, HasType b{-, HasExpr a, HasExpr b-}) =>
           -- (Show a, Show b) =>
           E (a,b) -> (E a, E b)
unPairE = fstE &&& sndE

instance UnitF E where unit = unitE
instance PairF E where (#)  = pairE

-- | Uniform version of a function on vectors
uniform :: (IsNat n, IsScalar a, Show a) =>
           (E (Vec n a) -> b) -> (E (One a) -> b)
uniform = (.  uniformV)

-- | Uniform vector
uniformV :: (IsNat n, IsScalar a, Show a) =>
            One a :=>* Vec n a
uniformV = fmapE (UniformV vectorT)


{--------------------------------------------------------------------
    AdditiveGroup and VectorSpace
--------------------------------------------------------------------}

instance (IsNat n, IsScalar a, Num a) =>
         AdditiveGroup (E (Vec n a)) where
  zeroV   = pureE  0
  (^+^)   = liftE2 Add
  negateV = fmapE  Negate

-- Hm.  Odd tension between Num & AdditiveGroup.  I'm avoiding adding
-- operators for AdditiveGroup and VectorSpace, so I won't have to add
-- rules for them.  Maybe just add the rules.

instance (IsNat n, IsScalar a, Num a) =>
         VectorSpace (E (Vec n a)) where
  type Scalar (E (Vec n a)) = E (One a)
  s *^ u                      = uniformV s * u
  -- (*^) = liftE2 Scale

instance IsNat n => InnerSpace (E (Vec n R)) where
  -- (<.>) = liftE2 (Dot nat)
  (<.>) = case (nat :: Nat n) of
            Succ Zero -> liftE2 Mul
            _         -> liftE2 Dot

-- Alternatively, I could eliminate the Scale operator and do a
-- scalar-to-vector expansion here, and maybe optimize away during code
-- generation.  Revisit.




{--------------------------------------------------------------------
    Conversion to expressions
--------------------------------------------------------------------}

-- | Turn a pattern into an expression.
patE :: Pat a -> E a
patE (BaseG v) = Var v
patE UnitG     = unitE
patE (p :* q)  = patE p # patE q



-- | Value convertible to an expression
class ToE w where
  type ExpT w
  toEN :: w -> NameM (E (ExpT w))

-- | Convert to an expression, using fresh name supply
toE :: ToE w => w -> E (ExpT w)
toE = runNameM . toEN

-- | Value convertible from an expression
class ToE w => FromE w where
  fromE :: E (ExpT w) -> w

instance ToE (E a) where
  type ExpT (E a) = a
  toEN = return
instance FromE (E a) where
  fromE = id

instance ToE () where
  type ExpT () = ()
  toEN () = return unit

instance FromE () where fromE = const ()

infixr 1 ##

(##) :: (PairF f, HasType a, HasType b {-, Show a, Show b -}) =>
        NameM (f a) -> NameM (f b) -> NameM (f (a,b))
(##) = liftM2 (#)

instance ( ToE u, Show (ExpT u), HasType (ExpT u)
         , ToE v, Show (ExpT v), HasType (ExpT v)
         ) => ToE (u,v) where
  type ExpT (u,v) = (ExpT u, ExpT v)
  toEN (u,v) = liftM2 (#) (toEN u) (toEN v)
               -- toEN u ## toEN v

instance ( FromE u {-, Show (ExpT u)-}, HasType (ExpT u)
         , FromE v {-, Show (ExpT v)-}, HasType (ExpT v)
         ) => FromE (u,v) where
  fromE e = (fromE eu, fromE ev) where (eu,ev) = unPairE e

instance ( ToE u {-, Show (ExpT u)-}, HasType (ExpT u)
         , ToE v {-, Show (ExpT v)-}, HasType (ExpT v)
         , ToE w {-, Show (ExpT w)-}, HasType (ExpT w)
         ) => ToE (u,v,w) where
  type ExpT (u,v,w) = ExpT u :# ExpT v :# ExpT w
  toEN (u,v,w) = toEN u ## toEN v ## toEN w

instance ( FromE u {-, Show (ExpT u)-}, HasType (ExpT u)
         , FromE v {-, Show (ExpT v)-}, HasType (ExpT v)
         , FromE w {-, Show (ExpT w)-}, HasType (ExpT w)
         ) => FromE (u,v,w) where
  fromE e = (fromE eu, fromE ev, fromE ew)
    where (eu,(ev,ew)) = (second unPairE . unPairE) e

instance (FromE u, ToE v, HasType (ExpT u)) => ToE (u -> v) where
  type ExpT (u -> v) = ExpT u -> ExpT v
  toEN f = do u <- genVar         -- p <- genPat
              b <- toEN (f (fromE (Var u))) -- patE p, or toE p
              return $ Lam u b

-- Hm.  Here I wish Lam allowed a Pat.  I'd then use genPat.  Revisit Lam.

{-
-- | Generate a pattern of the given type with new variable names
genPat :: HasType a => NameM (Pat a)
genPat = get' typeT
 where
   get' :: forall b. HasType b => Type b -> NameM (Pat b)
   get' UnitT     = return unit
   get' (a :*: b) = liftM2 (#) (get' a) (get' b)
   get' t         = fmap   (BaseG . flip V t) genName

-- TODO: Give genPat a [Name] argument.  Use runNameMWith.

-}

-- Patterns
instance ToE (Pat a) where
  type ExpT (Pat a) = a
  toEN = return . patE


-- | Construct an 'E' transformer from an 'ExpT' transformer
toFromE :: (FromE v, FromE w) => (v -> w) -> (E (ExpT v) -> E (ExpT w))
toFromE = fromE ~> toE

-- TODO: Check uses of toFromE and consider whether the fresh name supply
-- could be problematic.


-- | Complex-valued expressions
type ComplexE a = Complex (E (One a))

instance (Show a, IsScalar a) => ToE (ComplexE a) where
  type ExpT (ComplexE a) = Two a
  toEN (x :+ y) = return $ x <+> y
instance (Show a, IsScalar a) => FromE (ComplexE a) where
  fromE c = getX c :+ getY c

