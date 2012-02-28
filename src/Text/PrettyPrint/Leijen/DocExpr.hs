{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Leijen.DocExpr
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  BSD
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Variation of Twan van Laarhoven's simple-reflect
-- <http://hackage.haskell.org/cgi-bin/hackage-scripts/package/simple-reflect>
-- 
-- Differences from Twan's version:
-- + Generates pretty-printings instead of strings
-- + No evaluation
-- + Removed overloadings that disagree with semantic versions (e.g., '(==)')
-- + A few stylistic tweaks
----------------------------------------------------------------------

module Text.PrettyPrint.Leijen.DocExpr
    ( -- * Construction
      Expr(..)
    , FromExpr(..)
    , lift, var, fun, apply, ($$), Associativity(..), op
    , lambdaX, letX, tupleX, ccall, dotX, onDoc
    , HasExpr(..), HasExprU(..)
    , prettyExpr
    -- * Utility
    , docParen
    ) where

import Data.Ratio (Ratio)

import Text.PrettyPrint.Leijen
import Text.PrettyPrint.Leijen.PrettyPrec


------------------------------------------------------------------------------
-- Data type
------------------------------------------------------------------------------

-- | A reflected expression
data Expr = Expr
   { edoc :: Int -> Doc  -- ^ Generate doc, given contextual precedence level
   }

instance Pretty     Expr where pretty     = prettyPrec 0
instance PrettyPrec Expr where prettyPrec = flip edoc
instance Show       Expr where showsPrec  = showsPretty


------------------------------------------------------------------------------
-- Lifting and combining expressions
------------------------------------------------------------------------------

-- | A variable with the given name
var :: String -> Expr
-- var s = Expr (const (text s))
var = lift

lift :: PrettyPrec a => a -> Expr
lift x = Expr (\ p -> prettyPrec p x)

-- | This data type specifies the associativity of operators: left, right or none. 
data Associativity = InfixL | Infix | InfixR deriving Eq

-- | Generalization of 'op', taking a flag saying whether to insert spaces
-- around operator.
op' :: Bool -> Associativity -> Int -> String -> Expr -> Expr -> Expr
op' spaces fix prec name a b =
  withPrec prec $
  align (bump InfixL a `pre` text name `post` bump InfixR b)
 where
   bump fix' c = edoc c (if fix == fix' then prec else prec + 1)
   pre  | spaces                    = (<+>)
        | otherwise                 = (<>)
   post | spaces && not (null name) = (</>)
        | otherwise                 = (<>)

-- | An infix operator with the given associativity, precedence and name
op :: Associativity -> Int -> String -> Expr -> Expr -> Expr
op = op' True

-- | Variant of showParen
docParen       :: Bool -> Doc -> Doc
docParen True  = parens
docParen False = id

withPrec :: Int -> Doc -> Expr
withPrec n b = Expr $ \ p -> docParen (p > n) b

-- | A lambda expression
lambdaX :: String -> Expr -> Expr

lambdaX x body = withPrec 0 $
                 char '\\' <+> text x <+> text "->" <+> pretty body

-- | A \"let\" expression
letX :: String -> Expr -> Expr -> Expr
letX x rhs body = withPrec 0 $ hang 2 $
                   text "let" <+> text x <+> equals <+> pretty rhs
                   <+> text "in " <$$> pretty body

-- | A tuple expression
tupleX :: [Expr] -> Expr
tupleX = Expr . const . tupled . map (flip edoc 0)

-- | C-style call
ccall :: String -> [Expr] -> Expr
ccall f args = withPrec 9 $ text f <> edoc (tupleX args) 0

-- | e.foo
dotX :: String -> Expr -> Expr
dotX str e = op' False InfixR 10 "." e (var str)

-- dotX str (Expr d) = withPrec 10 $ d <> char '.' <> text str)

-- | Altering the generated Doc
onDoc :: (Doc -> Doc) -> (Expr -> Expr)
onDoc f (Expr ed) = Expr (f . ed)

------------------------------------------------------------------------------
-- Function types
------------------------------------------------------------------------------

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


infixr 0 $$

-- | Function application
apply, ($$) :: Expr -> Expr -> Expr
apply = op InfixL 10 ""

($$) = apply

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

-- Value that can be converted to an 'Expr'.  The 'PrettyPrec' parent is for
-- convenience.  It lets us use a default for 'expr'.
class PrettyPrec a => HasExpr a where
  expr :: a -> Expr
  -- expr = var . show
  expr e = Expr (\ i -> prettyPrec i e)

--   expr e = Expr (flip prettyPrec e)
--   expr = Expr . flip prettyPrec

-- prettyPrec :: Int -> a -> Doc

-- Grab instances from PrettyPrec:

instance HasExpr Expr    where expr = id

instance HasExpr Doc     where expr = lift
instance HasExpr ()      where expr = lift
instance HasExpr Bool    where expr = lift
instance HasExpr Char    where expr = lift
instance HasExpr Int     where expr = lift
instance HasExpr Integer where expr = lift
instance HasExpr Float   where expr = lift
instance HasExpr Double  where expr = lift

instance PrettyPrec a => HasExpr [a]
  where expr = lift
instance (Pretty a,Pretty b) => HasExpr (a,b) where
  expr = lift
instance (Pretty a,Pretty b,Pretty c) => HasExpr (a,b,c) where
  expr = lift
instance PrettyPrec a => HasExpr (Maybe a) where expr = lift
instance (Show a, Integral a) => HasExpr (Ratio a) where expr = lift

-- Price to pay for assuming HasExpr is a superclass of HasType. Revisit.
instance HasExpr (a -> b) where
  expr = error "DocExpr: can't really pretty a function. Sorry."


-- Like 'HasExpr', but for type constructors.
class HasExprU h where
  exprU :: forall a. {-HasExpr a => -} h a -> Expr

-- instance HasExpr a => PrettyPrec (V a) where
--   prettyPrec p v = edoc (expr v) p

-- | Convenient for defining 'PrettyPrec' when we have a 'HasExpr'.
prettyExpr :: HasExpr a => Int -> a -> Doc
prettyExpr p x = edoc (expr x) p
