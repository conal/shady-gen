{-# LANGUAGE TypeOperators, ScopedTypeVariables, ExistentialQuantification
  #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.CompileE
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Generate and compile vertex and fragment shaders.
-- 
-- In this version, shader programs are represented by functions from
-- a single expression to a single expression.  See also CompileEs, which
-- allows functions between more flexible representations.
----------------------------------------------------------------------

module Shady.CompileE
  ( -- VShaderF, FShaderF, SProgramF(..)
    Pos, (:->)(..), ShaderVF
  , GLSL(..), shaderProgram
  -- , ShaderExe(..), sinker, compile
  ) where

-- import Control.Applicative (liftA3)

import Text.PrettyPrint.Leijen
import qualified Text.PrettyPrint.Leijen as L
import Text.PrettyPrint.Leijen.PrettyPrec (PrettyPrec)
import Text.PrettyPrint.Leijen.DocExpr

-- import Shady.Misc (Sink)
import Shady.Language.Glom
import Shady.Language.Exp
import Shady.Language.GLSL
-- import Shady.Color (Color)

{-
import Shady.MechanicsGL (setupShader,glUseProgram,glMaxTextureUnits)
import Shady.Uniform
import Shady.Attribute
-}

-- | For gl_Position
type Pos = R4


{--------------------------------------------------------------------
    Generate and compile shader programs
--------------------------------------------------------------------}

infixr 7 :->, :-^, :-*

-- | Vertex shader
type a :-^ v = a :=>* (Pos,v)

-- | Fragment shader
type v :-* o = v :=>* (R4,o)

-- type v :--> o = v :=>* (R4,o)

-- | For building vertex/fragment shader pairs.  The idea is that a
-- complete parameterized shader program has type @u :=> a :- v :--> o@,
-- which expands to @u :=> (a :-^> v, v :-* o)@.
-- 
-- u == uniform, a == (vertex) attribute, v == varying, o == fragment output.
-- 
-- When @o == ()@ (color-only output), use the short-hand @u :=> a :-> v@.

-- | General vertex/fragment shader pair.
data a :-> o = forall v. (HasType v, HasExpr v, PrettyPrec v) =>
               ShaderVF (a :-^ v) (v :-* o)

-- | Vertex/fragment pair with no extra output besides color
type ShaderVF a = a :-> ()

-- | GLSL vertex program, fragment program, uniform and vertex attribute.
data GLSL u a = GLSL String String (Pat u) (Pat a)

instance (HasExpr u, HasExpr a) => Pretty (GLSL u a) where
  pretty (GLSL v f u a) = announce "vertex " v <$> announce "fragment" f
                          <$> pretty (u,a)
   where
     announce l sh = text (l ++ ": ") L.<+> align (pretty sh)

instance (HasExpr u, HasExpr a) => Show (GLSL u a) where
  show = show . pretty

-- | Compile a parameterized shader program.  TODO: generalize to non-()
-- outputs, i.e., to @u :=> a :-> o@.
shaderProgram :: (HasType a, HasExpr a, HasType u, HasExpr u) =>
                 (u :=> ShaderVF a) -> GLSL u a
shaderProgram uav =
  case uav (patE u) of
    ShaderVF vert frag ->
      let v = pat "_varying"
          
          vertOut = vert (patE a)
          fragOut = frag (patE v)
          
          uD = D [ Uniform ] u
          aD = D [Attribute] a
          vD = D [ Varying ] v
          
          vsh = shader [uD,aD,vD] (glPosition  :* v    ) vertOut
          fsh = shader [uD,   vD] (glFragColor :* UnitG) fragOut
      in
          GLSL (show vsh) (show fsh) u a
 where
   -- Uniform/varying variables
   u = pat "_uniform"
   a = pat "_attribute"

-- The awkward "case" keeps ghc's brain from exploding.

-- TODO: What do we want to do when o /= ()?

shader :: (HasExpr a, HasType a) => [Declaration] -> Pat a -> E a -> Shader
shader decls p e = Sh decls [mainDef (p =: e)]

{-

-- | Executable shader
data ShaderExe u a =
  ShaderExe { xSelect :: IO ()           -- ^ install this exe
            , xSinkU  :: Sink u          -- ^ set uniform
            , xsinkA  :: Sink [a]        -- ^ set attribute
            } 

sinker :: GLSL u a -> IO (ShaderExe u a)
sinker (GLSL vsh fsh u a) =
  do p     <- setupShader vsh fsh
     units <- glMaxTextureUnits
     return $
       ShaderExe (glUseProgram p) (setUniform units u p) (setAttribute a p)

{-
-- | Compile a parameterized shader program.  Set up a static (for now)
-- vertex mesh, and give a sink for setting uniforms and rendering.
compile :: (HasType a, HasExpr a, HasType u, HasExpr u) =>
           (u :=> ShaderVF a) -> IO () -> [a] -> IO (Sink u)
compile shf draw as =
  sinker (shaderProgram shf) >>= renderSE draw as

renderSE :: IO () -> [a] -> ShaderExe u a -> IO (Sink u)
renderSE draw as (ShaderExe useProg setU setA) =
  do useProg
     setA as
     return $ \ u -> useProg >> setU u >> draw

-- TODO: Maybe eliminate ShaderExe, collapsing sinker & renderSE into
-- compile

-}


-- | Compile a parameterized shader program.  Set up a static (for now)
-- vertex mesh, and give a sink for setting uniforms and rendering.
compile :: (HasType a, HasExpr a, HasType u, HasExpr u) =>
           (u :=> ShaderVF a) -> IO () -> [a] -> IO (Sink u)
compile shf draw as =
  do -- print (pretty g)
     p     <- setupShader vsh fsh
     units <- glMaxTextureUnits
     let useProg = glUseProgram        p
         setA    = setAttribute     pa p
         setU    = setUniform units pu p
     useProg
     setA as
     return $ \ u -> useProg >> setU u >> draw
 where
   GLSL vsh fsh pu pa = shaderProgram shf


-- TODO: switch from Sink [a] to Sink (Vbos a), so that the [a] -> Vbos
-- conversion can be done up front.  Vbos = Glom Vbo.  Then simplify the
-- signature to Vbos a -> u -> IO ().

-- For now I'm wiring in a fixed mesh.

-}
