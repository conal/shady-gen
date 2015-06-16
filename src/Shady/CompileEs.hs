{-# LANGUAGE TypeOperators, ScopedTypeVariables, TypeFamilies
           , FlexibleContexts, ExistentialQuantification, GADTs
  #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.CompileEs
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Generate and compile vertex and fragment shaders.  Unlike
-- "Shady.CompileE", this version allows a looser structure to
-- the inputs & outputs of shaders, according to 'FromE'.  You can use
-- the types and 'compile' in this module, or just 'shaders', along with
-- "Shady.CompileE", e.g., @compile (shaders sh)@.
----------------------------------------------------------------------

module Shady.CompileEs
  ( shaders
  , Pos, (:-^), (:-*), (:->)(..), ShaderVF
  , GLSL
  , shaderProgram
  -- , ShaderExe(..), compile
  ) where

import Text.PrettyPrint.Leijen.PrettyPrec (PrettyPrec)
import Text.PrettyPrint.Leijen.DocExpr (HasExpr)

import Shady.Language.Exp
import qualified Shady.CompileE as C
import Shady.CompileE (Pos, GLSL)
-- import Shady.CompileE (ShaderExe(..))
-- import Shady.Misc (Sink)

{--------------------------------------------------------------------
    Generate and compile shader programs
--------------------------------------------------------------------}

infixr 7 :->, :-^, :-*

-- | Vertex shader
type a' :-^ v'  = a' -> (E Pos,v')

-- | Fragment shader
type v' :-* o' = v' -> (E R4,o')

-- | General vertex/fragment shader pair.
data a' :-> o' =
  forall v' v. ( FromE v', v ~ ExpT v'
               , HasType v, HasExpr v, PrettyPrec v ) =>
  ShaderVF (a' :-^ v') (v' :-* o')

-- | Vertex/fragment pair with no extra output besides color
type ShaderVF a' = a' :-> ()

-- | Convert loosely structured shaders into single-exp shader
shaders :: forall u' a' o'.
           (FromE u', FromE a', FromE o') =>
           (o ~ ExpT o',a ~ ExpT a',u ~ ExpT u') =>
           ( HasExpr o, HasType o, Show o) =>
           (u' -> (a' :-> o'))
        -> u :=> (a C.:-> o)
shaders f u = case f (fromE u) of
                ShaderVF vert frag ->
                  C.ShaderVF (toFromE vert) (toFromE frag)

-- | Compile a parameterized shader program.  TODO: generalize to non-()
-- outputs, i.e., to @u :=> a :-> o@.
shaderProgram :: forall u' a' u a.
                 ( FromE u', u ~ ExpT u', FromE a', a ~ ExpT a') =>
                 ( HasType a, HasExpr a, HasType u, HasExpr u ) =>
                 (u' -> ShaderVF a') -> GLSL u a
shaderProgram = C.shaderProgram . shaders

{-
-- | Compile a parameterized shader program.  Set up a static (for now)
-- vertex mesh, and give a sink for setting uniforms and rendering.
compile :: forall u' a' u a.
           ( FromE u', u ~ ExpT u', FromE a', a ~ ExpT a') =>
           ( HasType a, HasExpr a, HasType u, HasExpr u ) =>
           (u' -> ShaderVF a') -> IO () -> [a] -> IO (Sink u)
compile = C.compile . shaders
-}
