{-# LANGUAGE TypeOperators, ScopedTypeVariables
           , FlexibleContexts, TypeFamilies
  #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.CompileImage
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Compile a parameterized image
----------------------------------------------------------------------

module Shady.CompileImage (ImageB, imageBProg,imSurfB, eyePos) where

import Data.Derivative (powVal)

import qualified Shady.Vec as V
import Shady.Language.Type (R1,R2)
import Shady.Language.Exp ((:=>),pureE)
import Shady.Color -- (white,HasColor(..))
import Shady.Image (Image)
import Shady.CompileE (GLSL)
import Shady.ParamSurf (xyPlane)
import Shady.Lighting (intrinsic,view1)
import Shady.CompileSurface (EyePosE,SurfB,surfBProg)
import Shady.Misc (EyePos)


-- Built on top of RunSurface

-- | 2D animation
type ImageB c = R1 :=> Image c

eyePos :: EyePos
eyePos = (0, 0.75, 2.5)     -- tweak

eyePosE :: EyePosE
eyePosE = pureE (V.vec3 ex ey ez) where (ex,ey,ez) = eyePos

imSurfB :: HasColor c => ImageB c -> SurfB
imSurfB imb t = (intrinsic, view1, xyPlane , toColor . imb (powVal t))

-- | GLSL program for an 'ImageB'.
imageBProg :: HasColor c => ImageB c -> GLSL R1 R2
imageBProg = surfBProg eyePosE . imSurfB
