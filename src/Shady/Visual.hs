{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Graphics.Shady.Visual
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPL-3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Type of display values
----------------------------------------------------------------------

module Graphics.Shady.Visual
  -- (  )
  where

import Data.Monoid (Monoid(..))
import Control.Monad (liftM2)

import Graphics.Rendering.OpenGL.GL (GLint)
-- import Graphics.UI.GLUT hiding (Shader,Program,Index)

import Graphics.Glew (glUseProgram)

import Data.NameM

import Graphics.Shady.Language.Exp hiding (ToE(..),FromE(..))
import Graphics.Shady.Language.Glom (Glom(..))
import Graphics.Shady.MechanicsGL (GlProgram)
import Graphics.Shady.Uniform (setUniform)
import Graphics.Shady.Misc ()  -- IO monoid

import Graphics.Shady.Color (HasColor(..))
import Graphics.Shady.Image (Image)
import Graphics.Shady.ParamSurf (SurfD)

-- import Graphics.Shady.Geometry (compileS)

-- First idea: any display operation.

-- | Visual object.  Can be displayed in an OpenGL context.  Expected to
-- generate Z-buffer as well as image, so that 'mappend' can interleave.
newtype Visual = Visual (IO ()) deriving Monoid

-- | Present/display a 'Visual'
present :: Visual -> IO ()
present (Visual io) = io

-- Or more declaratively
-- 
--   newtype Visual = Visual [([Bind], GlProgram)]
-- 
-- Or even a single, structured binding

shaderVis :: GLint -> IO () -> GlProgram -> Pat a -> (a -> Visual)
shaderVis texUnits display prog q = Visual . sink
 where
   sink a = do glUseProgram prog
               setU a
               display
   setU   = setUniform texUnits q prog

-- Note: no clear or swapBuffers, since we'll have more than one shaderVis


{--------------------------------------------------------------------
    Experimental.  To be factored out
--------------------------------------------------------------------}

data Geometry 
  = forall c. HasColor c => RenderG SurfD (Image c)
  | UnionG Geometry Geometry


{-

-- name supply.  move where?  TODO: Use name supply monad.  But don't
-- worry about name sharing between shader programs or between vertex &
-- fragment shaders in a given program.
type Names = [String]

renderG :: Geometry -> [Name] -> Visual
renderG (RenderG surf im) names = 
 where
   prog = compileS surf im names


compileS :: SurfD -> Image Color -> [Name] -> GlProgram
compileS = error "compileS"  -- See Shady.Geometry
-}

{-
class Compile w where
  type Compiled w
  compile :: w -> Compiled w

instance Compile Geometry where
  type Compiled Geometry = Visual

instance Compile w => Compile (E a -> w) where
  type Compiled (E a -> w) = a -> Compiled w
-}


-- TODO: Generalize this relationship between Geometry & Visual

-- instance Compile (E (a,b) -> w) => Compile (E a -> E b -> w) where
--   type Compiled (E a -> E b -> w) = Compiled ((E a, E b) -> w)
--   compile = curry . compile . uncurry


-- class Compile from to | from -> to where
--   compile :: from -> to

-- instance Compile (E a -> Geometry) (a -> Visual) where
--   -- ...

-- -- TODO: Generalize this relationship between Geometry & Visual

-- instance Compile (E (a,b) -> w) (E c -> z) =>
--          Compile (E a -> E b -> w) (E c -> z)

--   compile = curry . compile . uncurry


-- C (E a -> G) =  a -> V

-- C (E a -> E b -> G)  =  a -> b -> V

-- C G  =  V

-- C (E a -> w)  =  a -> C w


-- Plan:
-- 
--   uncurries to w -> G
--   (. fromE) to E (ExpT w) -> G
--   compile   to ExpT w -> V
--   curries   to ... -> V


-- TODO: move the next part to Exp.


-- f :: u -> v

-- u :: V (ExpT u)

-- Var u :: E (ExpT u)

-- fromE (Var u) :: u

-- f (fromE (Var u)) :: v

-- toEN (f (fromE (Var u))) :: NameM (E (ExpT v))

-- b :: E (ExpT v)

-- Lam u b :: E (ExpT u -> ExpT v)


