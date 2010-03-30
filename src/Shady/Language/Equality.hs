{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Graphics.Shady.Language.Equality
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPL-3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Forms of equality
----------------------------------------------------------------------

module Graphics.Shady.Language.Equality 
  -- (SynEq(..),SynEq2(..)) 
  where

{-
import Control.Applicative (Const(..))

infix 4 =-=, =--=

-- | Syntactic equality.  Requires same argument type.
class SynEq f where
  (=-=) :: HasType c => f c -> f c -> Bool

instance Eq x => SynEq (Const x) where (=-=) = (==)

-- | Higher-order variant of 'SynEq'.  Can be defined via '(=-=)', or vice versa.
class SynEq2 f where
  (=--=) :: SynEq v => f v c -> f v c -> Bool


deriving instance Eq a => Eq (Const a b)

-}
