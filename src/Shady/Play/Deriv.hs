{-# LANGUAGE TypeOperators, TypeFamilies
           , TypeSynonymInstances, FlexibleInstances, UndecidableInstances
  #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Play.Deriv
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPL-3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Playing with derivatives
----------------------------------------------------------------------

module Shady.Play.Deriv where

-- import Data.VectorSpace
-- import Data.MemoTrie
-- import Data.Basis
-- import Data.LinearMap
import Data.Derivative

-- For testing
import Text.PrettyPrint.Leijen.DocExprV (Expr,HasExpr(expr))

import Graphics.Shady.Language.Exp
import Graphics.Shady.ParamSurf


ex :: HasExpr a => a -> Expr
ex = expr

u,v :: ER
u = Var (variable "u")
v = Var (variable "v")

--     No instances for (HasBasis (ER),
--                       HasTrie (Basis (ER)))
--       arising from a use of `idD' at Play/Deriv.hs:34:4-6

z :: TR
z = idD u


der :: TR -> TR
der = (`derivAtBasis` ())

der1 :: TR -> ER
der1 = powVal . der

ed :: TR -> Expr
ed = ex . der1

-- d :: ER :-* (ER :> ER)

-- lapply :: (ER :-* (ER :> ER)) -> (ER -> ER)
-- lapply d :: ER -> ER



--   cos(u)
ta = sin z

--   cos(3.0 * (u * u * u)) * (3.0 * dot(vec2(u + u,u),u.xx))
tb = sin (3 * z^3)

--   dot(vec2(3.0,- (sin(3.0 * u))),vec2(cos(u),3.0))
tc = 3 * sin z + cos (3 * z)

--   let a = 3.0 * u in 
--     let b = 3.0 * sin(u) + cos(a) in 
--       let c = dot(vec2(3.0,- (sin(a))),vec2(cos(u),3.0)) in 
--         exp(b * b) * dot(vec2(c,b),vec2(b,c))
td = exp (tb*tb)

uv :: Exp R2
uv = Var (variable "uv")

es :: SurfD -> Expr
es = ex . ($ uv) . surfVN

-- tu, tv :: ER2 :~> ER
-- tu = fstD
-- tv = sndD

-- tu, tv :: T
-- tu = fstD (u,v)
-- tv = sndD (u,v)



te :: USurf
te = hfSurf (fst * snd)

--     (vec3(uv.x,uv.y,uv.x * uv.y)
--     ,let a = 1.0 / sqrt(dot(uv.yx,uv.yx) + 1.0) in 
--        vec3(a * - (uv.y),a * - (uv.x),a))

-- Without simplification:

--     (vec3(uv.x,uv.y,uv.x * uv.y)
--     ,let a = 1.0 * uv.y + uv.x * 0.0 in 
--        let b = 0.0 * uv.y + uv.x * 1.0 in 
--          let c = 0.0 * b - a * 1.0 in 
--            let d = a * 0.0 - 1.0 * b in 
--              let e = 1.0 / sqrt(c * c + d * d + 1.0) in 
--                vec3(e * c,e * d,e * 1.0))



-- (uv.x, uv.y, uv.x * uv.y)



-- (a.y, a.z) - (b.z,b.y)

-- a.yz - b.zy
-- a.zx - b.xz
-- a.xy - b.yx

tf :: USurf
tf = sphere1

--     (let a = uv.x * 0.5 * 6.2831855 in 
--        let b = -6.2831855 * uv.y in 
--          let c = cos(a) in 
--            vec3(c * cos(b),c * sin(b),sin(a))
--     ,let a = uv.x * 0.5 * 6.2831855 in 
--        let b = cos(a) in 
--          let c = -6.2831855 * uv.y in 
--            let d = b in 
--              let e = sin(c) in 
--                let f = cos(c) in 
--                  let g = f in 
--                    let h = - e in 
--                      let i = b.xx * (vec2(g,h) * vec2(-6.2831855,-6.2831855)) in 
--                        let j = vec2(3.1415927,3.1415927) in 
--                          let k = vec2(- d,d) * j * i in 
--                            let l = - (sin(a)) in 
--                              let m = dot(vec2(l,- l) * j * vec2(f,e),i) in 
--                                (1.0 / sqrt(dot(k,k) + m * m)).xxx * vec3(- d *
--                                                                          3.1415927 *
--                                                                          (b * (g * -6.2831855))
--                                                                         ,d *
--                                                                          3.1415927 *
--                                                                          (b * (h * -6.2831855))
--                                                                         ,m))

-- The ideal is that the normal and point are identical.  I achieved this
-- idea in Vertigo, using an approximation of AC matching and sin^2+cos^2==1.


-- without simplification:

--     (let a = 6.2831855 * uv.y in 
--        let b = uv.x * 0.5 * 6.2831855 in 
--          let c = sin(- a) in 
--            let d = cos(- a) in 
--              let e = cos(b) in 
--                vec3(e * d - 0.0 * c,0.0 * d + e * c,sin(b))
--     ,let a = 6.2831855 * uv.y in 
--        let b = uv.x * 0.5 * 6.2831855 in 
--          let c = sin(- a) in 
--            let d = cos(b) in 
--              let e = d in 
--                let f = cos(- a) in 
--                  let g = f in 
--                    let h = - (sin(b)) in 
--                      let i = - c in 
--                        let j = g * -6.2831855 in 
--                          let k = h * 0.0 in 
--                            let l = i * -6.2831855 in 
--                              let m = e * 3.1415927 in 
--                                let n = e * 0.0 in 
--                                  let o = g * -0.0 in 
--                                    let p = h * 3.1415927 in 
--                                      let q = i * -0.0 in 
--                                        let r = 0.0 * l + (k * c + d * j) in 
--                                          let s = 0.0 * q + (p * c + d * o) in 
--                                            let t = s * n - m * r in 
--                                              let u = p * f + d * q + -1.0 * (0.0 * o) in 
--                                                let v = k * f + d * l + -1.0 * (0.0 * j) in 
--                                                  let w = m * v - u * n in 
--                                                    let x = u * r - s * v in 
--                                                      let y = 1.0 / sqrt(t * t + w * w + x * x) in 
--                                                        vec3(y * t,y * w,y * x))


-- Suppose I work with Exp (Three R) instead of Three (Exp R).  I may be
-- able to keep the expressions smaller.

