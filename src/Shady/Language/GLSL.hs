{-# LANGUAGE ExistentialQuantification, GADTs #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Language.GLSL
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Abstract syntax for GLSL.  Evolving.
----------------------------------------------------------------------

-- Experimenting with typed syntax.

module Shady.Language.GLSL
  (
  -- * Syntax types
    Program(..), Shader(..), Declaration(..), Definition(..)
  , VectorT(..), ScalarT(..), Param(..), Id, Bind(..)
  , Statement(..), Qualifier(..)
  -- * Handy for building bindings
  , BindO, (=::), closeB
  -- * Utilities
  , (=:)
  , glPosition, glFragColor, vTrans, nTrans, mainDef
  )
  where

import Data.Monoid (Monoid(..))
-- import Data.Maybe  (maybe)
import Data.Char   (toLower)

import Data.VectorSpace (normalized)

import Text.PrettyPrint.Leijen
import Text.PrettyPrint.Leijen.PrettyPrec (showsPretty)
import Text.PrettyPrint.Leijen.DocExpr (expr)

import Control.Compose (result)

import Shady.Language.Operator (Op(Pair,Lit))
import Shady.Language.Exp hiding ((<+>),get)
import Shady.Language.Glom
import Shady.Misc (padTo)

-- Common sub-expression elimination. Work in progress. The Cse module is
-- fast but misses some sharing. Share is slow and thorough.
-- 
-- TODO: combine the two approaches, using the Cse implementation as a
-- first pass and the Share implementation as a second.

import Shady.Language.Cse (cse)
-- import Shady.Language.Share (cse)

{--------------------------------------------------------------------
    Syntax types
--------------------------------------------------------------------}

-- data Exists f = forall a. Exists (f a)

-- | Variable binding
data Bind = forall a. B (Pat a) (E a)

-- | Statement
data Statement
  = Assign Bind
  | LetS Bind Statement
  | SkipS
  | ThenS Statement Statement

-- | Storage qualifier
data Qualifier = Uniform | Attribute | Varying deriving (Show, Eq)

-- | Variable declaration/initialization.
data Declaration = forall a. D [Qualifier] (Pat a)

-- | formal parameter
data Param = forall n a. M (VectorT n a) Id

-- | Top-level definition
data Definition = forall n a. F (Maybe (VectorT n a)) Id [Param] Statement

-- | Shader
data Shader = Sh [Declaration] [Definition]

-- | Program: Vertex shader and Fragment shader
data Program = P { pVertex :: Shader, pFragment :: Shader }


instance Monoid Statement where { mempty = SkipS ; mappend = ThenS }


{--------------------------------------------------------------------
    Utilities
--------------------------------------------------------------------}

-- | The standard gl_Position variable, which must be set in a vertex shader
glPosition :: Pat R4
glPosition = pat "gl_Position"

-- | The standard gl_FragColor variable, which must be set in a fragment shader
glFragColor :: Pat R4
glFragColor = pat "gl_FragColor"

-- Transform
trans :: IsNat n => String -> VecE n R -> VecE n R
trans vname p = Var (var vname) * p

-- Transform and normalize
transNz :: IsNat n => String -> VecE n R -> VecE n R
transNz = (result.result) normalized trans

-- transNz vname p = normalized (trans vname p)

-- | Transform a vertex using the standard model/view matrix
vTrans :: E R4 -> E R4
vTrans = trans "gl_ModelViewProjectionMatrix"

-- | Transform a normal using the standard normal matrix
nTrans :: E R3 -> E R3
nTrans = transNz "gl_NormalMatrix"

-- HACK: the type of the view matrix above is inferred to be vec4 instead of
-- mat4x4.  This lie saves me from having to introduce matrices to
-- the representation.  If I use them elswhere, get honest.

-- | @main@ in a shader program.
mainDef :: Statement -> Definition
mainDef = F Nothing "main" []


{--------------------------------------------------------------------
    Generate code for an assigment.  May introduce new names and generate
    local bindings.
--------------------------------------------------------------------}

-- Because GLSL doesn't have expression-level "let", float all lets to the
-- top level before generating code.  There may be a more efficient way to
-- use locals.

infix 0 =:
-- | Assignment statement
(=:) :: HasType a => Pat a -> E a -> Statement
p =: e = p <-- cse e

(<--) :: Pat a -> E a -> Statement

--     p =: let v::t=a in b[v];
--      -->
--     { var t v=a ; p =: b[v] }
p <-- (Lam v b :^ a) = letS v a (p <-- b)

p <-- e = Assign (B p e)


letS :: V a -> E a -> Statement -> Statement
letS v e = LetS (B (BaseG v) e)


{--------------------------------------------------------------------
    Pretty-printing / code generation
--------------------------------------------------------------------}

-- TODO: Consider changing Assign to use Pat and E instead of Bind, since
-- they have different concrete syntax.  Hm.  What's the concrete syntax
-- for a variable, "vec v" or "v"?  Maybe accept context-dependent
-- concrete syntax.

-- TODO: CSE-friendly splitting for p :* q, e.g., a Let.

instance Pretty Bind where
  pretty = prettyB True

-- Pretty-print a binding, showing types if @withTypes@ is true
prettyB :: Bool -> Bind -> Doc
prettyB withTypes = pret
 where
   pret :: Bind -> Doc
   pret (B UnitG _)    = empty
   pret (B (p :* q) e) = pret (B p a) <$> pret (B q b)
     where (a,b) = unPair' e
   pret (B (BaseG (V name ty)) e) =
     mbty ty <> text name <+> equals <+> pretty e <> semi
   mbty ty | withTypes = prettyTy ty <> space
           | otherwise = empty

-- Variant that pads types for variable alignment
prettyTy :: Type t -> Doc
prettyTy = text . padTo (length "float") . show


unPair' :: (Show a, Show b) => E (a,b) -> (E a, E b)
unPair' (Op (Lit (a,b))) = (Op (Lit a), Op (Lit b))
unPair' (Op Pair :^ a :^ b) = (a,b)
unPair' p = error $ "unPair': " ++ show (expr p)

-- TODO: Sort out & eliminate this error situation.


instance Pretty Statement where
  pretty (Assign bind)    = prettyB False bind
  pretty (LetS bind stat) = pretty bind <$> pretty stat
  pretty SkipS            = empty
  pretty (s `ThenS` t)    = pretty s <$> pretty t

instance Pretty Qualifier where pretty = lshowPad qMax

qMax :: Int
qMax = length "attribute"

instance Pretty Declaration where
  pretty (D quals patt) = prettyD patt
   where
     prettyD :: Pat t -> Doc
     prettyD UnitG               = empty
     prettyD (p :* q)            = prettyD p <$> prettyD q
     prettyD (BaseG (V name ty)) = vcat' quals <+> pretty ty <+> text name <> semi

instance Pretty Param where
  pretty (M ty name) = pretty ty <+> pretty name

instance Pretty Definition where
  pretty (F mbty name params body) =
    maybe (text "void") pretty mbty <+> text name <+>
    tupled' params <+> scoped (pretty body)

instance Pretty Shader where
  pretty (Sh decls funs) = vcat' decls <$> vcat' funs

instance Pretty Program where
  pretty (P v f) = line <> announce "vertex " v <$> announce "fragment" f
   where
     announce l sh = text (l ++ ": ") <+> align (pretty sh)

-- The initial 'line' is just so that a 'show'n (not 'pretty'd) tuple with
-- 'Program' starts at column 0.


{--------------------------------------------------------------------
    
--------------------------------------------------------------------}

-- | Binding with open (exposed) type.  Build with '(=::)' and '(#)'.
data BindO a = BindO (Pat a) (E a)

-- | 'V' specialization of '(=:)'.
(=::) :: HasType a => V a -> E a -> BindO a
v =:: e = BindO (BaseG v) e

instance PairF BindO where
  BindO p u # BindO q v = BindO (p # q) (u # v)

-- | Close an open binding
closeB :: HasType a => BindO a -> Statement
closeB (BindO p e) = p =: e

-- TODO: Swap names '(=:)' and '(=::)' if '(=:)' becomes more popular.

{--------------------------------------------------------------------
    Show instances
--------------------------------------------------------------------}

instance Show Bind        where showsPrec = showsPretty
instance Show Statement   where showsPrec = showsPretty
instance Show Declaration where showsPrec = showsPretty
instance Show Param       where showsPrec = showsPretty
instance Show Definition  where showsPrec = showsPretty
instance Show Shader      where showsPrec = showsPretty
instance Show Program     where showsPrec = showsPretty

{--------------------------------------------------------------------
    Utility belt
--------------------------------------------------------------------}

-- Show, lower-casing the first char and padding
lshowPad :: Show a => Int -> a -> Doc
lshowPad n = text . onHead toLower . padTo n . show

-- handy variants
vcat', tupled' :: Pretty a => [a] -> Doc
vcat'   = vcat   . map pretty
tupled' = tupled . map pretty

-- Doc in a scope
scoped :: Doc -> Doc
scoped d = braces (nest 4 (line <> d) <> line)

-- The following alternative doesn't quite work, since the nesting happens
-- after the first line break and so doesn't apply to the first line.
-- 
--   scoped = braces . newlines . nest 4
--    where
--      -- Like braces, parens, ...
--      newlines :: Doc -> Doc
--      newlines = enclose line line

onHead :: (a -> a) -> [a] -> [a]
onHead f (a:as) = f a : as
onHead _ _      = error "onHead: empty list"
