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
-- Test auto vectorization
----------------------------------------------------------------------

module Shady.Play.VectorTest where

-- For testing
import Text.PrettyPrint.Leijen.DocExpr (Expr,HasExpr(expr))

import Data.Boolean

import Shady.Language.Exp
import Shady.Image
import Shady.Misc (fmod)


x :: HasExpr a => a -> Expr
x = expr

y :: Point -> Expr
y = expr . pointToR2


l,m,n :: FloatE
l = Var (var "l")
m = Var (var "m")
n = Var (var "n")

q,r :: R2E
q = Var (var "q")
r = Var (var "r")

t1 = m <+> m

-- vec2(2.0,3.0) * vec2(m,m)
t2 = 2 * m <+> 3 * m

-- (vec2(m,m) * vec2(q.x,q.y)
--  + vec2(n,- n) * vec2(q.y,q.x))
-- * vec2(l,l)

-- Without vectorization:

--     vec2((m * q.x + n * q.y) * l
--         ,(m * q.y + - n * q.x) * l)

t3 = (m * getX q - (- n * getY q)) * l  <+>
     (m * getY q - (  n * getX q)) * l

--     bvec2 d = lessThan(vec2(0.5,0.5)
--                       ,mod(vec2((c * _varying.x + a * _varying.y) * b
--                                ,(c * _varying.y + - a * _varying.x) * b)
--                           ,vec2(1.0,1.0)));


-- lessThan(vec2(0.5,0.5)
--         ,mod((cos(vec2(_uniform,_uniform))
--               * vec2(_varying.x,_varying.y)
--               + vec2(sin(_uniform),- sin(_uniform))
--                 * vec2(_varying.y,_varying.x))
--              / sin(vec2(_uniform,_uniform))
--             ,vec2(1.0,1.0))).y

-- TODO: rewrite cos(vec2(x,x)) to vec2(cos(x), cos(x)), i.e.,
-- cos . uniformV to uniformV . cos (etc).


sm = sin m

-- let a = sin(m) in 
--   vec2((a * q.x + n * q.y) * l
--       ,(a * q.y + - n * q.x) * a)

t4 = (sm * getX q - (- n * getY q)) * l  <+>
     (sm * getY q - (  n * getX q)) * sm

-- I guess CSE interferes with vectorization.  Unless I move lets out of
-- the way during general optimization.

-- let a = sin(m) in 
--   vec2(a * a,a * 3.0)
t5 = sm * sm <+> sm * 3

-- sin(vec2(m,m)) * vec2(2.0,3.0)
t6 = sm * 2 <+> sm * 3

-- let a = sin(m) in 
--   vec2(2.0,a) * vec2(a,3.0)
t7 = 2 * sm <+> sm * 3

-- let a = sin(m) in 
--   vec2(a,n) * vec2(l,a)
t8 = sm * l <+> n * sm

-- let a = sin(m) in 
--   vec2(a,2.0 + n) * vec2(l,a)
t9 = sm * l <+> (2 + n) * sm

-- let a = sin(m) in 
--   vec2(a,a) * vec2(l,a)
ta = sm * l <+> sm * sm

-- sin m * sin m

{-
(sin m * l) <+> (let a = sin m in a * a)

liftE2 Cat (sin m * l) (let a = sin m in a * a)


liftE2 o a b = simple2 o a b @> op2 o a b

liftE2 o (Lam r :^ s) b = Lam (liftE2 o r (down1 b)) ^: s

liftE2 o (let x = s in r x) b  ==  let x = s in liftE2 o (r x) b




op2 o a b = op1 o a @^ b
          = Op o a :^ a @^ b

-}


tb = q <* ((sm * 3) `fmod` n <+> 2)

tc = getX tb ==* getY tb

te = ifB tc 5 6 :: FloatE


sqr a = a * a

ti = getX q + 1
tj = getY q + 1

-- let a = q.x + 1.0 in a * a
tk = sqr ti
-- let a = q.y + 1.0 in a * a
tl = sqr tj

-- let a = q.xy + vec2(1.0,1.0) in dot(a,a)
tn = tk + tl
