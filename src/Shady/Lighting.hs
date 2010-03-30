{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Lighting
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Lighting/shading.  Adapted from Vertigo.
----------------------------------------------------------------------

module Shady.Lighting
  ( LightInfo(..), Light, View(..), SurfInfo(..)
  , Lighter, LLighter
  , ambient, eye, lights
  , surfP, surfN, intrinsic
  , colorL, dirL, illuminance
  , diffuse, ambDiff, eyeDir, eyeLight, reflection
  , specularG, specularRV, specularNH
  , BasicSh, basic, basicRV, basicNH
  , Liftable(..)
  , Dir3E, dirLight, pointLight
  , stdViewPos, stdView, view1
  , ma,md,ms,msh
  , basicStd
  ) where

import Data.VectorSpace (AdditiveGroup(..),(*^), sumV,(<.>),normalized)
import Data.Boolean

import Shady.Language.Exp
import Shady.Color


{--------------------------------------------------------------------
    Basic types
--------------------------------------------------------------------}

-- | Info about how one light affects a given point.  The surface lighter
-- decides what to do with the light info.  Attenuation and relation of
-- light position (if finitely distant) to surface position are already
-- accounted for.  'liDir' is the direction /to/ the light (normalized).
data LightInfo = LI { liColor :: Color, liDir :: Dir3E }

-- | A light is something that provides light info to every point in space
-- (though to some points it provides blackness), independent of
-- obstructions.  Should probably also take an atmosphere argument.
type Light = R3E -> LightInfo

-- | Viewing environment: ambient, eye, lights
data View =
  View { viewAmbient :: Color
       , viewEye     :: R3E
       , viewLights  :: [Light]
       }

-- | Info about a surface at a point: position, normal, color
data SurfInfo = SurfInfo { surfPos :: R3E, surfNormal :: Dir3E, surfColor :: Color }

-- Lighters are functions from contextual info to values.

-- | View-dependent lighter
type Lighter  a = View -> SurfInfo -> a

-- | Light- and view-dependent lighter
type LLighter a = LightInfo -> Lighter a


{--------------------------------------------------------------------
    Extractors
--------------------------------------------------------------------}


-- | Ambient color
ambient :: Lighter Color
ambient = lift . viewAmbient

-- ambient v _ = viewAmbient v
-- ambient v = const (viewAmbient v)

-- | Eye point
eye :: Lighter  R3E
eye = lift . viewEye

-- | Lights
lights :: Lighter [Light]
lights = lift . viewLights

-- | Surface point
surfP :: Lighter R3E
surfP = lift surfPos

-- | Surface normal
surfN :: Lighter Dir3E
surfN = lift surfNormal

-- | Surface Color
intrinsic :: Lighter Color
intrinsic = lift surfColor

-- | Light color
colorL :: LLighter Color
colorL = lift . liColor

-- | Direction /to/ light
dirL :: LLighter R3E
dirL = lift . liDir

-- | Combine contributions from multiple lights.  Patterned after
-- Renderman's @illuminance@ construct.
illuminance :: -- (Num a, VectorSpace IfB (VecE OneT Bool) (Scalar a), Num (Scalar a)) =>
               (AdditiveGroup a, IfB BoolE a) =>
               LLighter a -> Lighter a
illuminance llighter v@(View _ _ ls) s@(SurfInfo p _ _) =
  -- sumV [ (ifB (lift surfN <.> dirL >* 0) llighter zeroV) (light p) v s | light <- ls ]
  sumV [ llighter (light p) v s | light <- ls ]

-- bsign b = boolean 1 0 b

-- | Does this bsign multiplier really help?  The vertex engine clamps to
-- [0,1] anyway.  Oh, with multiple light sources, negative contributions
-- would subtract from positive ones.


{--------------------------------------------------------------------
    Composite lighters
--------------------------------------------------------------------}

-- Two-sided lighting?
twoSided :: Bool
twoSided = True

-- One-sided or two-sided lighting
sided :: (Ord a, Num a) => a -> a
sided | twoSided  = abs
      | otherwise = max 0

-- | Pure diffuse
diffuse :: Lighter Color
diffuse = illuminance (sided (lift surfN <.> dirL) *^ colorL)


-- | Weighted combination of ambient and diffuse
ambDiff :: (Color, Color) -> Lighter Color
ambDiff (ka,kd) = intrinsic * (lift ka * ambient + lift kd * diffuse)

-- | The Stanford rtsl version, with ambient and weights:
-- 
-- surface float4
-- lightmodel_diffuse (float4 ka, float4 kd)
-- {
--     perlight float diffuse = dot(N,L);
--     perlight float4 fr = kd * select(diffuse > 0, diffuse, 0);
--     return ka * Ca + integrate(fr * Cl);
-- }

-- | Direction from surface point to eye
eyeDir :: Lighter Dir3E
eyeDir = normalized (eye - surfP)

-- | Eye/light vector average (CGPP p 731)
eyeLight :: LLighter Dir3E
eyeLight = normalized (dirL + lift eyeDir)

-- | Reflection vector (CGPP p 730)
reflection :: LLighter Dir3E
reflection = (2 * (n' <.> dirL)) *^ n' - dirL  where n' = lift surfN

-- | Pure specular.  Ignores intrinsic surface color.  There are different
-- ways to compute the power base.
specularG :: LLighter FloatE -> FloatE -> Lighter Color
specularG base sh = illuminance ((max 0 base ** lift sh) *^ colorL)


-- Phong's specular (R.V) model
specularRV :: FloatE -> Lighter Color
specularRV = specularG (reflection <.> lift eyeDir)

-- | Or the N.H model:
specularNH :: FloatE -> Lighter Color
specularNH = specularG (lift surfN <.> eyeLight)


-- | surface floatv
-- lightmodel_specular (floatv s, floatv e, float sh)
-- {
--     perlight float diffuse = dot(N,DIRL);
--     perlight float specular = pow(max(dot(N,H),0),sh);
--     perlight floatv fr = select(diffuse > 0, s * specular, Zero);
--     return integrate(fr * Cl) + e;
-- }

type BasicSh = (Color,Color,Color,FloatE) -> Lighter Color

-- | Combine intrinsic, ambient, diffuse and specular, with weightings
basic :: LLighter FloatE -> BasicSh
basic base (ka,kd,ks,sh) = ambDiff (ka,kd) + lift ks * specularG base sh

basicRV, basicNH :: BasicSh
basicRV = basic (reflection <.> lift eyeDir)     -- The R.V model
basicNH = basic (lift surfN <.> eyeLight)        -- The N.H model

-- | surface float4
-- lightmodel (float4 a, float4 d, float4 s, float4 e, float sh)
-- {
--     perlight float diffuse = dot(N,DIRL);
--     perlight float specular = pow(max(dot(N,H),0),sh);
--     perlight float4 fr = d * max(diffuse, 0) +
--                          s * select(diffuse > 0, specular, 0);
--     return a * Ca + integrate(fr * Cl) + e;
-- }


{-
-- Phong model, taking normal as parameter
-- WORKING HERE.  Coefficients not yet right.

phongN :: LLighter DirE -> Lighter Color
phongN n = 
  cs * lift ka * ambient + cs * lift kd ^* illuminance ldotN
  + lift ks ^* illuminance (vdotR ** lift sh)
-}

{--------------------------------------------------------------------
    Lifting.  Revisit
--------------------------------------------------------------------}


class Liftable k f where lift :: k -> f

instance Liftable a a            where lift = id
instance Liftable b (a->b)       where lift = const
instance Liftable c (a->b->c)    where lift = const . const
instance Liftable d (a->b->c->d) where lift = const . const . const


{--------------------------------------------------------------------
    Some lights
--------------------------------------------------------------------}

-- | Direction.  Assumed normalized.
type Dir3E = R3E

-- | Directional light, given the direction /from/ the light (opposite 'dirL')
dirLight :: Color -> Dir3E -> Light
dirLight col dir = const (LI col (- dir))

-- | Point light
pointLight :: Color -> R3E -> Light
pointLight col lpos p = LI col (normalized (lpos - p))


-- To do: add distance-based fall-off and spot lights

stdViewPos :: R3E
stdViewPos = vec3 0.5 1 (-2) :: R3E

-- | View with white ambient light and given eye position and lights
stdView :: R3E -> [Light] -> View
stdView = View white

-- | View with white ambient light, one directional light, and given eye position.
-- For now, light position is like eye position but more so.
view1 :: R3E -> View
view1 eyePos = stdView eyePos [dirLight lightColor lightDir]
 where
   lightColor = white
   -- lightColor = rgb 1 0.9 0.5  -- light gold
   lightDir = normalized (-eyePos + vec3 2 1 0)

-- view2 :: Anim R3E -> Anim View
-- view2 pos t = stdView [pointLight red (pos t)]


-- Standard material properties (from rtsl-shaders.in)

ma,md,ms :: Color
msh :: FloatE

ma = gray 0.2                          -- ambient
md = gray 0.4                          -- diffuse
ms = gray 0.5                          -- specular
msh = 15                               -- specular exponent

{-
-- Simple shaders with standard material properties
ambDiffStd  = ambDiff ma md
specularStd = specularNH msh
-}

basicStd :: Lighter Color
basicStd = basicRV (ma,md,ms,msh)
