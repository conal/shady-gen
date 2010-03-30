-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
----------------------------------------------------------------------
-- |
-- Module      :  Play.CseTest
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPL-3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Test misc simplifications
----------------------------------------------------------------------

module Play.Simplify where


-- import Data.VectorSpace
import Text.PrettyPrint.Leijen.DocExprV (Expr,HasExpr(expr))

import Graphics.Shady.Language.Exp
import Graphics.Shady.Color3
import Graphics.Shady.Complex
-- import Graphics.Shady.Transform2
import Graphics.Shady.Image


x :: HasExpr a => a -> Expr
x = expr

y :: ColorE -> Expr
y = expr . colorToR4

z :: PointE -> Expr
z = expr . pointToR2


q,r :: Exp R1
q = Var (variable "q")
r = Var (variable "r")

w :: Exp R2
w = Var (variable "w")

t1 = 0 * q
t2 = q * 0

t3 = (1 - q) * 0
t4 = q * 1

c :: ComplexE R
c = q :+ r



