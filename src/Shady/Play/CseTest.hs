-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Play.CseTest
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Test new CSE stuff
----------------------------------------------------------------------

module Shady.Play.CseTest where

-- import Control.Applicative (liftA2)

import Data.VectorSpace
-- For testing
import Text.PrettyPrint.Leijen.DocExpr (Expr,HasExpr(expr))

import Data.Boolean

import Shady.Language.Exp
-- import Shady.Color
-- import Shady.Image
import Shady.Complex
import Shady.Misc (frac)

-- import Shady.Language.Cse
import Shady.Language.Share

x :: HasExpr a => a -> Expr
x = expr


type Point = ComplexE R


{-
xc :: Color -> Expr
xc = expr . colorToR4

xp :: Point -> Expr
xp = expr . pointToR2

-}

q :: FloatE
q = Var (var "q")

t1,t2 :: FloatE
t1 = q + q

-- Was @q * (q + q)@, now @let a = q + q in a * a@.  What happened?
t2 = t1 * t1

c1 = cse t1

t3a = sin q / cos q

--     let a = sin(q) in 
--       let b = cos(q) in 
--         b + a / b
-- 
t3 = cos q + t3a

-- cse => cos(q) + sin(q) / cos(q)


t3b = cq + sq / cq
 where
   cq = cos q
   sq = sin q

-- cse => let x3 = cos(q) in x3 + sin(q) / x3

--     let a = cos(q) in 
--       a - 1.0 / a
-- 
t4 = cos q - 1 / cos q

-- let a = cos(q) in 
--   a * (a + sin(q) / a)
-- 
t5 = cos q * t3

-- let a = cos(q) in 
--   (a + sin(q) / a) * (a - 1.0 / a)
-- 
t6 = t3 * t4


-- let a = cos(q) in 
--   let b = sin(q) in 
--     (a + b / a) * (a - 1.0 / a) + (a + b / a)

t7  = t6 + t3

-- let a = sin(q) in 
--   a + (1.0 - a) * (a < 3.0 ? 4.0 : 5.0)
-- 
t8 = let a = sin q in a + (1 - a) * (ifE (a <* 3) 4 5)

-- q * sin(q)
r = q * sin q

-- let a = sin(q) in 
--   a * (q * a)
s = sin q * r

-- let a = sin(q) in 
--   let b = q * a in 
--     b + a * b
t9a = r + s


-- let a = sin(q) in 
--   let b = q * a in 
--     a * b + b
t9b = s + r


w = Var (var "w") :: R2E

{-

bw :: BoolE -> Color
bw = boolean white clear


ra :: R2E -> Color
ra z = bw (z <.> z <* 1)
-}

stripes (a :+ _) = frac a <* 0.5

a1 :: FloatE
a1 = magnitudeSq (t *^ uv)

{-
a2 :: BoolE
a2 = uscale2 t udisk uv

a3 :: R4E
a3 = colorToR4 $ toColor (uscale2 (cos t) udisk uv)
-}


t :: FloatE
t = Var (var "t")
u,v :: FloatE
u = Var (var "u")
v = Var (var "v")


uv :: Point
uv = u :+ v

-------------

ts = [t1,t2,t3a,t3,t4,t5,t6,t8,t9a,t9b]

main = mapM_ (print.expr) ts
