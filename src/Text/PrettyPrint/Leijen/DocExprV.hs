{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Leijen.DocExprV
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  BSD
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Variant of "Text.PrettyPrint.Leijen.DocExpr".  This one manages a name
-- supply.
----------------------------------------------------------------------

### UNUSED ###

module Text.PrettyPrint.Leijen.DocExprV
    ( -- * Construction
      Expr(..), withNewName
    , FromExpr(..)
    , lift, var, fun, apply, ($$), Associativity(..), op
    , letX, lambdaX, tupleX, ccall, dotX, onDoc
    , HasExpr(..), HasExprU(..)
    , prettyExpr
    ) where

import Control.Applicative ((<$>))
import Data.Ratio (Ratio)

import Control.Monad.State (State(..),evalState,liftM2)
import qualified Control.Monad.State as S
import Control.Monad.State.Class (get,put)

import Text.PrettyPrint.Leijen hiding ((<$>))
import Text.PrettyPrint.Leijen.PrettyPrec
import qualified Text.PrettyPrint.Leijen.DocExpr as X
import Text.PrettyPrint.Leijen.DocExpr (Associativity(..))

import Graphics.Shady.Misc (result,(~>))


{--------------------------------------------------------------------
    Name supply
--------------------------------------------------------------------}

type Supply = [String]

-- a, ... z, aa, ab, ... az, ba, bb, ..., zz, aaa ...
allNames :: Supply
allNames = map reverse (tail supply)
 where
   supply = "" : [c:s | s <- supply , c <- ['a'..'z']]


------------------------------------------------------------------------------
-- Data type
------------------------------------------------------------------------------

-- | Representation of 'Expr'
type Expr' = State Supply X.Expr

-- | A reflected expression
data Expr = Expr { unExpr :: Expr' }

instance X.HasExpr  Expr where expr (Expr ex) = evalState ex allNames
instance PrettyPrec Expr where prettyPrec     = X.prettyExpr
instance Pretty     Expr where pretty         = prettyPrec 0
instance Show       Expr where showsPrec      = showsPretty

inX :: (Expr' -> Expr') -> (Expr -> Expr)
inX = unExpr ~> Expr

inX2 :: (Expr' -> Expr' -> Expr') -> (Expr -> Expr -> Expr)
inX2 = unExpr ~> inX

-- | Allocate an unused name
genName :: State Supply String
genName = do (name:names) <- get
             put names
             return name

-- | Supply a name
withNewName :: (String -> Expr) -> Expr
withNewName f = Expr (genName >>= (unExpr . f))

------------------------------------------------------------------------------
-- Lifting and combining expressions
------------------------------------------------------------------------------

-- | A variable with the given name
var :: String -> Expr
var = lift . X.var

lift :: PrettyPrec a => a -> Expr
lift = Expr . return . X.lift

-- | Altering the generated Doc
onDoc :: (Doc -> Doc) -> (Expr -> Expr)
onDoc f (Expr st) = Expr (X.onDoc f <$> st)

-- | An infix operator with the given associativity, precedence and name
op :: Associativity -> Int -> String -> Expr -> Expr -> Expr
op = (result.result.result) (inX2.liftM2) X.op

-- Or:
-- 
--   op fix prec name (Expr sta) (Expr stb) =
--     Expr (liftM2 (X.op fix prec name) sta stb)

-- | A \"let\" expression
letX :: Expr -> (String -> Expr) -> Expr
letX a f = withNewName h
 where
   h name = Expr $ liftM2 (X.letX name) (unExpr a) (unExpr (f name))

-- | A lambda expression
lambdaX :: (String -> Expr) -> Expr
lambdaX f = withNewName h
 where
   h name = Expr $ fmap (X.lambdaX name) (unExpr (f name))

-- | A tuple expression
tupleX :: [Expr] -> Expr
tupleX = Expr . fmap X.tupleX . mapM unExpr

-- | C-style call
ccall :: String -> [Expr] -> Expr
ccall f = Expr . fmap (X.ccall f) . mapM unExpr

-- | e.foo
dotX :: String -> Expr -> Expr
dotX str = inX (fmap (X.dotX str))



------------------------------------------------------------------------------
-- Function types
------------------------------------------------------------------------------

infixr 0 $$
-- | Function application
apply, ($$) :: Expr -> Expr -> Expr

-- ($$) = (inX2.liftM2) X.apply

-- Expr f $$ Expr x = Expr (liftM2 X.apply f x)

Expr f $$ Expr x = Expr $
  do f' <- f
     x' <- x
     return (X.apply f' x')

apply = ($$)

-- | Conversion from 'Expr' to other types
class FromExpr a where
    fromExpr :: Expr -> a

instance FromExpr Expr where
    fromExpr = id

instance (PrettyPrec a, FromExpr b) => FromExpr (a -> b) where
    fromExpr f a = fromExpr (f $$ lift a)

-- | A generic, overloaded, function variable
fun :: FromExpr a => String -> a
fun = fromExpr . var


------------------------------------------------------------------------------
-- Numeric classes
------------------------------------------------------------------------------

-- The types of some methods prevent them from being lifted to Expr
noOv :: String -> a
noOv meth = error $ meth ++ ": No overloading for Expr"

instance Eq Expr where
  -- (==) = (==) `on` show 
  (==) = noOv "(==)"

instance Ord Expr where
  -- compare = compare `on` show
  compare = noOv "compare"
  min = fun "min"
  max = fun "max"

instance Num Expr where
  fromInteger = lift
  (+)    = op InfixL 6 "+"
  (-)    = op InfixL 6 "-"
  (*)    = op InfixL 7 "*"
  negate = fun "negate"
  abs    = fun "abs"
  signum = fun "signum"

instance Real Expr where
  toRational = noOv "toRational"

instance Integral Expr where
  toInteger   = noOv "toInteger"
  quotRem a b = (quot a b, rem a b)
  divMod  a b = (div  a b, mod a b)
  quot        = op InfixL 7 "`quot`"
  rem         = op InfixL 7 "`rem`"
  div         = op InfixL 7 "`div`"
  mod         = op InfixL 7 "`mod`"

instance Fractional Expr where
  (/)          = op InfixL 7 "/"
  recip        = fun "recip"
  fromRational = lift

instance Floating Expr where
  pi    = var "pi"
  exp   = fun "exp"
  sqrt  = fun "sqrt"
  log   = fun "log"
  (**)  = op InfixR 8 "**"
  sin   = fun "sin"
  cos   = fun "cos"
  sinh  = fun "sinh"
  cosh  = fun "cosh"
  asin  = fun "asin"
  acos  = fun "acos"
  atan  = fun "atan"
  asinh = fun "asinh"
  acosh = fun "acosh"
  atanh = fun "atanh"

instance Enum Expr where
  succ           = fun  "succ"
  pred           = fun  "pred"
  toEnum         = fun  "toEnum"
  fromEnum       = noOv "fromEnum"
  enumFrom       = noOv "enumFrom"
  enumFromThen   = noOv "enumFromThen"
  enumFromTo     = noOv "enumFromTo"
  enumFromThenTo = noOv "enumFromThenTo"



{--------------------------------------------------------------------
    HasExpr Class: conversion to Expr
--------------------------------------------------------------------}

-- TODO: sync up names FromExpr and HasExpr

-- Value that can be converted to an 'Expr'.  The 'Show' parent is for
-- convenience.  It lets us use a default for 'expr'.
class Show a => HasExpr a where
  expr :: a -> Expr
  expr = var . show

-- Grab instances from X.HasExpr:

instance HasExpr Doc     where expr = lift
instance HasExpr ()      where expr = lift
instance HasExpr Bool    where expr = lift
instance HasExpr Char    where expr = lift
instance HasExpr Int     where expr = lift
instance HasExpr Integer where expr = lift
instance HasExpr Float   where expr = lift
instance HasExpr Double  where expr = lift

instance (PrettyPrec a, Show a) => HasExpr [a]
  where expr = lift
instance (PrettyPrec a, PrettyPrec b, Show a,Show b) => HasExpr (a,b) where
  expr = lift
instance (PrettyPrec a,PrettyPrec b,PrettyPrec c,Show a,Show b,Show c) => HasExpr (a,b,c) where
  expr = lift
instance (PrettyPrec a, Show a) => HasExpr (Maybe a) where expr = lift
instance Integral a => HasExpr (Ratio a) where expr = lift


-- Like 'HasExpr', but for type constructors.
class HasExprU h where
  exprU :: forall a. {-HasExpr a => -} h a -> Expr


-- | Convenient for defining 'PrettyPrec' when we have a 'HasExpr'.
prettyExpr :: HasExpr a => Int -> a -> Doc
prettyExpr p x = X.edoc (X.expr (expr x)) p
