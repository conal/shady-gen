{-# LANGUAGE Rank2Types, TypeOperators, FlexibleContexts, TypeFamilies
           , TypeSynonymInstances, FlexibleInstances, UndecidableInstances
           , MultiParamTypeClasses
  #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.ParamSurf
-- Copyright   :  (c) Conal Elliott 2008, 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Parametric surfaces with automatic normals
----------------------------------------------------------------------

-- This version uses Complex s instead of (s,s).  Complex is consistent
-- with Image but inconsistent with 1D and 3D.

module Shady.ParamSurf where

import Control.Applicative
import Control.Arrow ((&&&))

import Data.NumInstances ()
import Data.VectorSpace
import Data.Cross hiding (One,Two,Three)
import Data.Derivative

-- import Data.MemoTrie
import Data.Basis

import Shady.Language.Exp
import Shady.Complex
import Shady.ITransform (ITrans(..))


type HeightField s = Complex s -> s
type Surf        s = Complex s -> (s,s,s)

type USurf = forall s. Floating s => Surf s


type Curve2 s = s         -> Complex s
type Curve3 s = s         -> (s,s,s)

type Warp1  s = s         -> s
type Warp2  s = Complex s -> Complex s
type Warp3  s = (s,s,s)   -> (s,s,s)

-- | Trig functions with unit period ([-1,1])
cosU, sinU :: Floating s => s -> s
cosU = cos . (* pi)
sinU = sin . (* pi)


-- | Turn a height field into a surface
hfSurf :: HeightField s -> Surf s
hfSurf field = \ (u :+ v) -> (u, v, field (u :+ v))

-- | Like 'hfSurf' but for curve construction
fcurve :: Warp1 s -> Curve2 s
fcurve f = \ u -> u :+ f u

-- | Unit circle.
circle :: Floating s => Curve2 s
circle = liftA2 (:+) cosU sinU

-- | Half semi circle, with theta in [-pi/2,pi/2]
semiCircle :: Floating s => Curve2 s
semiCircle = circle . (/ 2)

-- | Torus, given radius of sweep circle and cross section
torus :: (Floating s, VectorSpace s, Scalar s ~ s) => s -> s -> Surf s
-- torus sr cr = revolve (\ s -> (sr,0) ^+^ cr *^ circle s)
torus sr cr = revolve (const (sr :+ 0) ^+^ const cr *^ circle)

-- Surface of revolution, formed by rotation around Z axis.  The curve is
-- parameterized by u, and the rotation by v.  In this generalized
-- version, we have not a single curve, but a function from v to curves.
revolveG :: Floating s => (s -> Curve2 s) -> Surf s
revolveG curveF = \ (u :+ v) -> onXY (rotate (-pi*v)) (addY (curveF v) u)

revolve :: Floating s => Curve2 s -> Surf s
revolve curve = revolveG (const curve)

-- A sphere is a revolved semi-circle
sphere1 :: Floating s => Surf s
sphere1 = revolve semiCircle


-- | Profile product.
profile :: Num s => Curve2 s -> Curve2 s -> Surf s
profile curve prof (u :+ v) = (cx*px,cy*px,py)
 where
   cx :+ cy = curve u
   px :+ py = prof  v

-- More spheres
sphere2,sphere3 :: Floating s => Surf s
sphere2 = profile circle semiCircle
sphere3 = profile semiCircle circle

-- | Frustum, given base & cap radii and height.
frustum :: (Floating s, VectorSpace s, Scalar s ~ s) => s -> s -> s -> Surf s
frustum baseR topR h = profile circle rad
 where
   rad t = lerp baseR topR (t + 1/2) :+ h*t

-- | Unit cylinder.  Unit height and radii
ucylinder :: (Floating s, VectorSpace s) => Surf s
ucylinder = profile circle (const 1)

-- | XY plane as a surface
xyPlane :: Num s => Surf s
xyPlane = hfSurf (const 0)

-- | Given a combining op and two curves, make a surface.  A sort of
-- Cartesian product with combination.
cartF :: (a -> b -> c) -> (s -> a) -> (s -> b) -> (Complex s -> c)
cartF op f g = \ (u :+ v) -> f u `op` g v

-- Sweep a basis curve by a sweep curve.  Warning: does not reorient the
-- basis curve as cross-section.  TODO: Frenet frame.
sweep :: VectorSpace s => Curve3 s -> Curve3 s -> Surf s
sweep = cartF (^+^)


-- | One period, unit height eggcrate
eggcrateH :: Floating s => HeightField s
eggcrateH = cartF (*) cosU sinU

revolveH :: (Floating s, InnerSpace s, Scalar s ~ s) => Warp1 s -> HeightField s
revolveH = (. magnitude)

rippleH :: (Floating s, InnerSpace s, Scalar s ~ s) => HeightField s
rippleH = revolveH sinU

-- | Simple ripply pond shape
ripple :: Floating s => Surf s
ripple = -- onXY' (2 *^) $
         revolve (const (0.5 :+ 0) - fcurve sinU)

-- | Apply a displacement map at a value

displaceV :: (InnerSpace v, s ~ Scalar v, Floating s, HasNormal v) =>
             v -> Scalar v -> v
displaceV v s = v ^+^ s *^ normal v

-- | Apply a displacement map to a function (e.g., 'Curve2' or 'Surf') or
-- other container.
displace :: (InnerSpace v, Scalar v ~ s, Floating s, HasNormal v, Applicative f) =>
            f v -> f (Scalar v) -> f v
displace = liftA2 displaceV



---- Misc

-- TODO: Reconcile this version with the one in Image

rotate :: Floating s => s -> Warp2 s
rotate theta = \ (x :+ y) -> (x * c - y * s) :+  (y * c + x * s)
 where c = cos theta
       s = sin theta

addX, addY, addZ :: Num s => (a -> Complex s) -> (a -> (s,s,s))
addX = fmap (\ (y :+ z) -> (0,y,z))
addY = fmap (\ (x :+ z) -> (x,0,z))
addZ = fmap (\ (x :+ y) -> (x,y,0))

addYZ,addXZ,addXY :: Num s => (a -> s) -> (a -> (s,s,s))
addYZ = fmap (\ x -> (x,0,0))
addXZ = fmap (\ y -> (0,y,0))
addXY = fmap (\ z -> (0,0,z))

onX,onY,onZ :: Warp1 s -> Warp3 s
onX f (x,y,z) = (f x, y, z)
onY f (x,y,z) = (x, f y, z)
onZ f (x,y,z) = (x, y, f z)

onXY,onYZ,onXZ :: Warp2 s -> Warp3 s
onXY f (x,y,z) = (x',y',z ) where x' :+ y' = f (x :+ y)
onXZ f (x,y,z) = (x',y ,z') where x' :+ z' = f (x :+ z)
onYZ f (x,y,z) = (x ,y',z') where y' :+ z' = f (y :+ z)


onX',onY',onZ' :: Warp1 s -> (a -> (s,s,s)) -> (a -> (s,s,s))
onX' = fmap . onX
onY' = fmap . onY
onZ' = fmap . onZ

onXY',onXZ',onYZ' :: Warp2 s -> (a -> (s,s,s)) -> (a -> (s,s,s))
onXY' = fmap . onXY
onXZ' = fmap . onXZ
onYZ' = fmap . onYZ


{--------------------------------------------------------------------
    Normals and tessellation
--------------------------------------------------------------------}

-- -- | Derivative tower of point on a surface
-- type SurfPt = Exp R2 :> Exp R3

-- -- | Differentiable surface
-- type SurfD = Surf (Exp R2 :> Exp R)

-- -- | Vertex and normal
-- data VN = VN (Exp R3) (Exp R3)

-- --     No instances for (HasBasis (E V R2),
-- --                       HasTrie (Basis (E V R2)),
-- --                       HasNormal SurfPt)

-- toVN :: SurfPt -> VN
-- toVN v = VN (powVal v) (powVal (normal v))


-- TODO: move to Exp and remove -fno-warn-orphans


type V2 a = (a,a)
type V3 a = (a,a,a)

type ER  = FloatE
type ER2 = V2 ER
type ER3 = V3 ER

instance HasBasis FloatE where
  type Basis FloatE = ()
  basisValue ()     = 1
  decompose  s      = [((),s)]
  decompose' s      = const s

instance HasBasis R2E where
  type Basis R2E = Basis ER2
  basisValue b   = vec2 x y where (x,y) = basisValue b
  decompose  w   = decompose (getX w, getX w)
  decompose' w   = (w <.>) . basisValue

-- TODO: are these instances used?

-- TODO: move these two HasBasis orphans elsewhere.

-- instance IsNat n => HasBasis (VecE n R) where
--   type Basis (VecE n R) = n
--   basisValue            = ???

-- TODO: fill out this definition.  How to enumerate a basis for Vec n R,
-- for arbitrary IsNat n?

type TR = ER :> ER -- tower

type T = ER2 :> ER

-- Standard do-nothing transformation
instance ITrans (Complex T) T where (*:) = const id


-- | Derivative towers of point on a surface
type SurfPt = V3 T

-- type SurfPt = ER2 :> ER3


-- | Differentiable surface
type SurfD = Surf T


-- -- | Vertex and normal
-- data VN = VN ER3 ER3

-- powVal3 :: V3 (a :> b) -> V3 b
-- powVal3 (q,r,s) = (powVal q, powVal r, powVal s)

-- toVN :: SurfPt -> VN
-- toVN v = VN (powVal3 v) (powVal3 (normal v))

-- -- type SurfV = ER2 :~> ER3

-- type SurfVN = ER2 -> VN

-- -- or
-- --   type SurfVN = Exp R2 -> (Exp R3, Exp R3)

-- surfVN :: SurfD -> SurfVN
-- surfVN f p = toVN (f (fstD p, sndD p))


-- | Vertex and normal
type VN = (R3E, R3E)

toVN :: SurfPt -> VN
toVN = p3 &&& (p3 . normal)
 where
   p3 (q,r,s) = vec3 (powVal q) (powVal r) (powVal s)

-- type SurfV = ER2 :~> ER3

type SurfVN = R2E -> VN

surfVN :: SurfD -> SurfVN
surfVN f p = toVN (f (fstD p' :+ sndD p'))
 where
   p' = (getX p, getY p)
