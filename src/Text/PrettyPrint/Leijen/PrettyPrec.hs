{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Leijen.PrettyPrec
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  BSD
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Pretty class with precedence
----------------------------------------------------------------------

module Text.PrettyPrint.Leijen.PrettyPrec
  ( PrettyPrec(..)
  -- * 'Show' helpers
  , showsPretty, showsPretty'
  , showsPrettyPrec, showsPrettyPrec'
  ) where

#if __GLASGOW_HASKELL__ < 612
import Data.Maybe (maybe)
#endif
import Data.Ratio (Ratio)

import Text.PrettyPrint.Leijen

-- | Pretty printing with precedence.  A cross between 'Show' and 'Pretty'.
-- The 'prettyPrec' method defaults to discarding the context precedence
-- and invoking 'pretty'.  The reason 'PrettyPrec' derives from Pretty is
-- that so that this default is possible.
-- 
-- To make a 'Show' instance for a 'PrettyPrec' instance 'Foo', define
-- 
--   instance Show Foo where showsPrec p e = showsPrec p (prettyPrec p e)

class Pretty a => PrettyPrec a where
  prettyPrec :: Int -> a -> Doc
  prettyPrec = const pretty  -- default
  
-- Will we need prettyListPrec?
-- 
--   prettyList   :: [a] -> Doc
--   prettyList    = list . map pretty


instance PrettyPrec Doc
instance PrettyPrec ()
instance PrettyPrec Bool
instance PrettyPrec Char
instance PrettyPrec Int
instance PrettyPrec Integer
instance PrettyPrec Float
instance PrettyPrec Double

-- Orphan. Missing from wl-pprint
instance Show a => Pretty (Ratio a) where pretty = text . show

instance Pretty a => PrettyPrec [a]

instance (Pretty a,Pretty b) => PrettyPrec (a,b)

instance (Pretty a,Pretty b,Pretty c) => PrettyPrec (a,b,c)

instance PrettyPrec a => PrettyPrec (Maybe a) where
  prettyPrec p = maybe empty (prettyPrec p)

instance Show a => PrettyPrec (Ratio a) where
  prettyPrec = const (text . show)

-- TODO: Revisit Ratio. Use p

-- Price to pay for assuming HasExpr is a superclass of HasType. Revisit.
instance Pretty (a -> b) where
  pretty = error "PrettyPrec: can't really pretty a function. Sorry."
instance PrettyPrec (a -> b)


{--------------------------------------------------------------------
    'Show' helpers
--------------------------------------------------------------------}

pageWidth :: Int
pageWidth = 80

-- | Convenient definition for 'showsPrec' in a 'Show' instance.  Uses
-- ribbon fraction of 0.9 and width of 80.  To set these values, use
-- 'showsPrettyPrec'' instead.  See also 'showsPretty'.
showsPrettyPrec :: PrettyPrec a => Int -> a -> ShowS
showsPrettyPrec = showsPrettyPrec' 0.9 pageWidth

-- | Convenient definition for 'showsPrec' in a 'Show' instance.
-- Arguments are ribbon fraction and line width.  To get my defaults, use
-- 'showsPrettyPretty' instead.
showsPrettyPrec' :: PrettyPrec a => Float -> Int -> Int -> a -> ShowS
showsPrettyPrec' rfrac w p = showsG (prettyPrec p) rfrac w

-- | Convenient definition for 'showsPrec' in a 'Show' instance.  Uses
-- ribbon fraction of 0.9 and width of 80.  To set these values, use
-- 'showsPretty'' instead.  If you want to take precedence into account,
-- use 'showsPrettyPrec' instead.
showsPretty :: Pretty a => Int -> a -> ShowS
showsPretty = showsPretty' 0.9 pageWidth

-- | Convenient definition for 'showsPrec' in a 'Show' instance.
-- Arguments are ribbon fraction and line width.  To get my defaults, use
-- 'showsPretty' instead.  Ignores precedence, which 'Pretty' doesn't
-- understand.  If you have a 'PrettyPrec' instance, you can use
-- 'showsPrettyPrec' instead.
showsPretty' :: Pretty a => Float -> Int -> Int -> a -> ShowS
showsPretty' rfrac w _ = showsG pretty rfrac w

-- General 'Doc'-friendly helper for 'showsPrec' definitions.
showsG :: (a -> Doc) -> Float -> Int -> a -> ShowS
showsG toDoc rfrac w a = displayS (renderPretty rfrac w (toDoc a))
