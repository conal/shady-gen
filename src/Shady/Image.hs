{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts
           , TypeSynonymInstances, MultiParamTypeClasses, Rank2Types
  #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Image
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Images (infinite & continuous)
----------------------------------------------------------------------

-- This variation uses Complex

module Shady.Image
  (
    Point, pointToR2, r2ToPoint
  , FilterG, Filter, samplerIm, scale2, uscale2, translate2, rotate2
  , bilerp, bilerpC
  , ImageG, Image
    -- * General regions
  , PRegion, Region
  , universeR, emptyR, eqF, neqF, intersectR, unionR, xorR, diffR, complementR
  , udisk, disk, annulus, checker
  , crop
    -- * Space-varying transformations
  , transformG, translate2Im, scale2Im, uscale2Im, rotate2Im
  , swirl -- , uswirl
  , utile, tile
  ) where

import Control.Applicative (Applicative(..),liftA2)
import Shady.Complex

import Data.VectorSpace

import Data.Boolean

import Shady.Misc
import Shady.Language.Exp
import Shady.Color
import Shady.ITransform

type Point = ComplexE R

pointToR2 :: Point -> R2E
pointToR2 (x :+ y) = vec2 x y

r2ToPoint :: R2E -> Point
r2ToPoint xy = getX xy :+ getY xy

-- | Generalized image -- continuous & infinite
type ImageG s a = Complex s -> a

-- | Continuous, infinite image
type Image a = ImageG FloatE a


-- == Point -> a

-- | Generalized filter, polymorphic over domain
type FilterG p a = Unop (p -> a)

-- | Image filter
type Filter a = FilterG Point a
                 -- Unop (Image a)


-- | Wrap up a sampler as an image
samplerIm :: Sampler2 :=> Image Color
samplerIm s = r4ToColor . texture s . pointToR2


-- -- | 2D invertible transform
-- type ITransform2 = ITransform Point

translate2X :: AdditiveGroup a => a         -> ITransform a
scale2X     :: Fractional    s => Complex s -> ITransform (Complex s)
uscale2X    :: Fractional    s => s         -> ITransform (Complex s)
rotate2X    :: Floating      s => s         -> ITransform (Complex s)

translate2X = andInverse (^+^) negateV
scale2X     = andInverse (onRI2 (*)) (onRI recip)
rotate2X    = andInverse rotate2C     negate
uscale2X    = scale2X . \ a -> a :+ a

rotate2C :: Floating s => s -> Unop (Complex s)
rotate2C theta = (cis theta *)

-- experiment

translate2, scale2 :: (Floating s, ITrans (Complex s) a) => Complex s -> Unop a
uscale2,rotate2    :: (Floating s, ITrans (Complex s) a) => s -> Unop a

translate2 = (*:) . translate2X
scale2     = (*:) . scale2X
rotate2    = (*:) . rotate2X
uscale2    = (*:) . uscale2X

-- translate2 :: ITransform Point
-- (*:) :: ITransform w -> Unop a

-- (*:) . translate2 :: ITransform Point
--  :: ITransform w -> Unop a


-- | Bilinear interpolation
bilerp :: VectorSpace w =>
          w -> w -> w -> w -> (Scalar w, Scalar w) -> w
bilerp ll lr ul ur (dx,dy) =
  lerp (lerp ll lr dx) (lerp ul ur dx) dy

-- | Bilinear interpolation image
bilerpC :: (VectorSpace w, Scalar w ~ s) =>
           w -> w -> w -> w -> ImageG s w
bilerpC ll lr ul ur (dx :+ dy) = bilerp ll lr ul ur (dx,dy)


{--------------------------------------------------------------------
    Generalized regions
--------------------------------------------------------------------}

-- TODO: Move most of these definitions elsewhere, since they're not
-- specific to 2D.

-- | Region over general space
type PRegion p = p -> BoolE

-- | 2D spatial region
type Region = Image BoolE

universeR, emptyR :: Applicative f => f BoolE
universeR = pure true
emptyR    = pure false

eqF, neqF :: (IsNat n, IsScalar a, Eq a, Applicative f) =>
             f (VecE n a) -> f (VecE n a) -> f BoolE

eqF  = liftA2 (==^)
neqF = liftA2 (/=^)

-- intersectR, unionR, xorR, diffR
--   :: LiftA2 BoolE BoolE BoolE b b b => b -> b -> b
-- complementR :: LiftA1 BoolE BoolE b b => b -> b

intersectR, unionR, xorR, diffR :: Applicative f => Binop (f BoolE)
complementR                     :: Applicative f => Unop  (f BoolE)

intersectR  = liftA2 (&&*)
unionR      = liftA2 (||*)
complementR = fmap notE
xorR        = neqF

diffR r r' = r `intersectR` complementR r'


-- | Generalized unit disk/ball
udisk :: (InnerSpace p, Scalar p ~ FloatE) => PRegion p
udisk p = magnitudeSq p <=* 1

-- | Generalized disk/ball, given radius
disk :: (InnerSpace p, Scalar p ~ FloatE) => FloatE -> PRegion p
disk s = udisk . (^/ s)

-- | Generalized annulus, given outer & inner radii
annulus :: (InnerSpace p, Scalar p ~ FloatE) => FloatE -> FloatE -> PRegion p
annulus o i = disk o `diffR` disk i


-- | Checker-board
checker :: Region
checker (x :+ y) = getX c ==* getY c
  where c = frac (x <+> y) >* 0.5

-- checker (x :+ y) = big x ==* big y
--  where
--    big = (>* 0.5) . frac


{--------------------------------------------------------------------
    Some generalized transforms
--------------------------------------------------------------------}

-- | General domain-varying transformation.
transformG' :: (c -> Unop p) -> (p -> c) -> Unop (p -> a)
transformG' f imc ima p = ima (f (imc p) p)

-- transformG' :: (c -> Unop Point) -> Image c -> Filter a

-- | General domain-varying transformation.
transformG :: (c -> ITransform p) -> (p -> c) -> Unop (p -> a)
transformG f = transformG' (itBackward . f)

-- transformG :: (c -> ITransform2) -> Image c -> Filter a

-- translate2Im :: Image Point -> Filter a
-- scale2Im :: Image Point -> Filter a
-- uscale2Im :: Image FloatE -> Filter a
-- rotate2Im :: Image FloatE -> Filter a


-- | Space-varying 'translate2'
translate2Im :: AdditiveGroup p => Unop p -> Unop (p -> a)
translate2Im = transformG translate2X

-- | Space-varying 'scale2'
scale2Im :: Fractional s => Unop (Complex s) -> Unop (ImageG s a)
scale2Im = transformG scale2X

-- | Space-varying 'uscale2'
uscale2Im :: Fractional s => ImageG s s -> Unop (ImageG s a)
uscale2Im = transformG uscale2X

-- | Space-varying 'rotate2'
rotate2Im :: Floating s => ImageG s s -> Unop (ImageG s a)
rotate2Im = transformG rotate2X


{--------------------------------------------------------------------
    Other transformations
--------------------------------------------------------------------}

-- -- | Unit swirl
-- uswirl :: Filter a
-- uswirl = rotate2Im magnitude

-- -- | Swirl transformation
-- swirl :: FloatE -> Filter a
-- swirl s = hyperUscale2 s uswirl

-- *Almost* equivalent, but differs for negative s.

-- | Swirl transformation
swirl :: Floating s => s -> Unop (ImageG s a)
swirl s = rotate2Im ((2*pi*s*) . magnitude)

utile' :: Frac p => Unop (p -> a)
utile' = (. frac)

-- Hm!  This utile' definition repeats [0,1), not [-.5,.5).  Eep.  How can
-- I shift without loss of generality?  For instance, the current
-- definition can handle nD.

-- | Unit, rectangular tiling.
utile :: (Frac p, ITrans (Complex s) p, ITrans (Complex s) a, Floating s) => 
         Unop (p -> a)
utile = translate2 (negate (0.5 :+ 0.5)) utile'

-- TODO: Generalize uniform scaling to arbitrary vector spaces, scaling
-- via scalar field.

-- Rectangle tiling with given size.
-- tile :: ITrans Point a => Point -> Filter a

tile :: (Floating s, Frac s, ITrans (Complex s) a) =>
        Complex s -> Unop (ImageG s a)
tile s = scale2 s utile

-- tile = flip scale2 utile


{--------------------------------------------------------------------
    Orphans
--------------------------------------------------------------------}

-- Standard do-nothing transformation
instance ITrans Point Color where (*:) = const id
