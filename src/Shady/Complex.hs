{-# LANGUAGE TypeOperators, CPP, DeriveDataTypeable, TypeFamilies, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Complex
-- Copyright   :  (c) The University of Glasgow 2001, Conal Elliott 2009
-- License     :  BSD-style
--
-- Maintainer  :  conal@conal.net
-- Stability   :  provisional
-- Portability :  portable
--
-- Complex numbers.  This version is modified from Data.Complex in base.
-- It eliminates the RealFloat requirement by using a more naive
-- definition of 'magnitude'.  Also, defines instances for vector-space classes.
--
-----------------------------------------------------------------------------

module Shady.Complex
        (
        -- * Rectangular form
          Complex((:+))

        , realPart      -- :: Complex a -> a
        , imagPart      -- :: Complex a -> a
        -- * Polar form
        , mkPolar       -- :: a -> a -> Complex a
        , cis           -- :: a -> Complex a
        , polar         -- :: Complex a -> (a,a)
        -- , magnitude     -- :: Complex a -> a
        , phase         -- :: Complex a -> a
        -- * Conjugate
        , conjugate     -- :: Complex a -> Complex a

        -- Complex instances: (Eq,Read,Show,Num,Fractional,Floating)
        -- Complex instances: (AdditiveGroup, VectorSpace, InnerSpace)

        -- * Misc interface additions
        , onRI, onRI2
        )  where

import Prelude

import Data.Typeable
#ifdef __GLASGOW_HASKELL__
import Data.Data (Data)
#endif

#ifdef __HUGS__
import Hugs.Prelude(Num(fromInt), Fractional(fromDouble))
#endif

import Data.VectorSpace

import Shady.Misc (Unop,Binop,FMod(..),Frac(..))
import Text.PrettyPrint.Leijen (Pretty(..),text)
import Text.PrettyPrint.Leijen.DocExpr
import Text.PrettyPrint.Leijen.PrettyPrec (PrettyPrec(..))

infix  6  :+

-- -----------------------------------------------------------------------------
-- The Complex type

-- | Complex numbers are an algebraic type.
--
-- For a complex number @z@, @'abs' z@ is a number with the magnitude of @z@,
-- but oriented in the positive real direction, whereas @'signum' z@
-- has the phase of @z@, but unit magnitude.
data Complex a
  = !a :+ !a    -- ^ forms a complex number from its real and imaginary
                -- rectangular components.
# if __GLASGOW_HASKELL__
        deriving (Eq, Show, Read, Data)
# else
        deriving (Eq, Show, Read)
# endif

-- -----------------------------------------------------------------------------
-- Functions over Complex

-- | Extracts the real part of a complex number.
realPart :: Complex a -> a
realPart (x :+ _) =  x

-- | Extracts the imaginary part of a complex number.
imagPart :: Complex a -> a
imagPart (_ :+ y) =  y

-- | The conjugate of a complex number.
{-# SPECIALISE conjugate :: Complex Double -> Complex Double #-}
conjugate        :: Num a => Unop (Complex a)
conjugate (x:+y) =  x :+ (-y)

-- | Form a complex number from polar components of magnitude and phase.
{-# SPECIALISE mkPolar :: Double -> Double -> Complex Double #-}
mkPolar          :: Floating a => a -> a -> Complex a
mkPolar r theta  =  r * cos theta :+ r * sin theta

-- | @'cis' t@ is a complex value with magnitude @1@
-- and phase @t@ (modulo @2*'pi'@).
{-# SPECIALISE cis :: Double -> Complex Double #-}
cis              :: Floating a => a -> Complex a
cis theta        =  cos theta :+ sin theta


-- | The function 'polar' takes a complex number and
-- returns a (magnitude, phase) pair in canonical form:
-- the magnitude is nonnegative, and the phase in the range @(-'pi', 'pi']@;
-- if the magnitude is zero, then so is the phase.
{-# SPECIALISE polar :: Complex Double -> (Double,Double) #-}
polar            :: Floating a => Complex a -> (a,a)
polar z          =  (magnitude z, phase z)


-- | Operate on the real & imaginary components
onRI :: Unop a -> Unop (Complex a)
onRI f (x :+ y) = f x :+ f y

-- | Operate on the real & imaginary components
onRI2 :: Binop a -> Binop (Complex a)
onRI2 f (x :+ y) (x' :+ y') = f x x' :+ f y y'

instance Floating a => AdditiveGroup (Complex a) where
  { zeroV = 0 ; negateV = negate ; (^+^) = (+) }

instance Floating a => VectorSpace (Complex a) where
  type Scalar (Complex a) = a
  -- s *^ (x :+ y) = s * x :+ s * y
  (*^) s = onRI (s *)

instance Floating a => InnerSpace (Complex a) where
  (x :+ y) <.> (x' :+ y') = x*x' + y*y'


{-

-- | The nonnegative magnitude of a complex number.
{-# SPECIALISE magnitude :: Complex Double -> Double #-}
magnitude :: Floating a => Complex a -> a
magnitude = sqrt . magSq

magnitudeSq :: Floating a => Complex a -> a

-- magnitude (x:+y) =  scaleFloat k
--                      (sqrt (sqr (scaleFloat mk x) + sqr (scaleFloat mk y)))
--                     where k  = max (exponent x) (exponent y)
--                           mk = - k
--                           sqr z = z * z

-}

-- | The phase of a complex number, in the range @(-'pi', 'pi']@.
-- If the magnitude is zero, then so is the phase.
{-# SPECIALISE phase :: Complex Double -> Double #-}
phase :: Floating a => Complex a -> a
-- The zero case requires a real EQ instance
-- phase (0 :+ 0)   = 0            -- SLPJ July 97 from John Peterson
phase (x:+y)     = atan2' y x

-- To avoid reliance on 'RealFloat'.
atan2' :: (Floating a) => a -> a -> a
atan2' y x = atan (y/x)

-- -----------------------------------------------------------------------------
-- Instances of Complex

#include "Typeable.h"
INSTANCE_TYPEABLE1(Complex,complexTc,"Complex")

instance Floating a => Num (Complex a)  where
    {-# SPECIALISE instance Num (Complex Float) #-}
    {-# SPECIALISE instance Num (Complex Double) #-}
    (x:+y) + (x':+y')   =  (x+x') :+ (y+y')
    (x:+y) - (x':+y')   =  (x-x') :+ (y-y')
    (x:+y) * (x':+y')   =  (x*x'-y*y') :+ (x*y'+y*x')
    negate (x:+y)       =  negate x :+ negate y
    abs z               =  magnitude z :+ 0
    signum (0:+0)       =  0
    signum z@(x:+y)     =  x/r :+ y/r  where r = magnitude z
    fromInteger n       =  fromInteger n :+ 0
#ifdef __HUGS__
    fromInt n           =  fromInt n :+ 0
#endif

instance Floating a => Fractional (Complex a)  where
    {-# SPECIALISE instance Fractional (Complex Float) #-}
    {-# SPECIALISE instance Fractional (Complex Double) #-}

    (x:+y) / v@(x':+y')   =  ((x*x'+y*y') :+ (y*x'-x*y')) ^/ magnitudeSq v

--     (x:+y) / (x':+y')   =  (x*x''+y*y'') / d :+ (y*x''-x*y'') / d
--                            where x'' = scaleFloat k x'
--                                  y'' = scaleFloat k y'
--                                  k   = - max (exponent x') (exponent y')
--                                  d   = x'*x'' + y'*y''

    fromRational a      =  fromRational a :+ 0
#ifdef __HUGS__
    fromDouble a        =  fromDouble a :+ 0
#endif

instance Floating a => Floating (Complex a) where
    {-# SPECIALISE instance Floating (Complex Float) #-}
    {-# SPECIALISE instance Floating (Complex Double) #-}
    pi             =  pi :+ 0
    exp (x:+y)     =  expx * cos y :+ expx * sin y
                      where expx = exp x
    log z          =  log (magnitude z) :+ phase z

--     x ** y =  exp (log x * y)
--     sqrt   =  (** 0.5)

    -- Use default sqrt (** 0.5)

--     sqrt (0:+0)    =  0
--     sqrt z@(x:+y)  =  u :+ (if y < 0 then -v else v)
--                       where (u,v) = if x < 0 then (v',u') else (u',v')
--                             v'    = abs y / (u'*2)
--                             u'    = sqrt ((magnitude z + abs x) / 2)

    sin (x:+y)     =  sin x * cosh y :+ cos x * sinh y
    cos (x:+y)     =  cos x * cosh y :+ (- sin x * sinh y)
    tan (x:+y)     =  (sinx*coshy:+cosx*sinhy)/(cosx*coshy:+(-sinx*sinhy))
                      where sinx  = sin x
                            cosx  = cos x
                            sinhy = sinh y
                            coshy = cosh y

    sinh (x:+y)    =  cos y * sinh x :+ sin  y * cosh x
    cosh (x:+y)    =  cos y * cosh x :+ sin y * sinh x
    tanh (x:+y)    =  (cosy*sinhx:+siny*coshx)/(cosy*coshx:+siny*sinhx)
                      where siny  = sin y
                            cosy  = cos y
                            sinhx = sinh x
                            coshx = cosh x

    asin z@(x:+y)  =  y':+(-x')
                      where  (x':+y') = log (((-y):+x) + sqrt (1 - z*z))
    acos z         =  y'':+(-x'')
                      where (x'':+y'') = log (z + ((-y'):+x'))
                            (x':+y')   = sqrt (1 - z*z)
    atan z@(x:+y)  =  y':+(-x')
                      where (x':+y') = log (((1-y):+x) / sqrt (1+z*z))

    asinh z        =  log (z + sqrt (1+z*z))
    acosh z        =  log (z + (z+1) * sqrt ((z-1)/(z+1)))
    atanh z        =  log ((1+z) / sqrt (1-z*z))


{--------------------------------------------------------------------
    Pretty printing
--------------------------------------------------------------------}

instance RealFloat a => Pretty (Complex a) where
  pretty = text . show

instance RealFloat a => PrettyPrec (Complex a)
  -- default

-- TODO: Revisit this instance. Use p

-- infix  6  :+
instance (RealFloat a, HasExpr a) => HasExpr (Complex a) where
  expr (x :+ y) = op Infix 6 ":+" (expr x) (expr y)

-- TODO: Do I really need HasExpr for Complex? I don't generate them in code.


{--------------------------------------------------------------------
    Misc
--------------------------------------------------------------------}

instance Frac s => Frac (Complex s) where frac = onRI frac
instance FMod s => FMod (Complex s) where fmod = onRI2 fmod
