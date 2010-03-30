{-# LANGUAGE MultiParamTypeClasses , FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Graphics.Shady.Cat
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPL-3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Catenable vectors
----------------------------------------------------------------------

module Graphics.Shady.Cat (Catenable2(..), Catenable3(..)) where

import Graphics.Shady.Vec

{--------------------------------------------------------------------
    Catenation
--------------------------------------------------------------------}

class Catenable2 a b c {- | a b -> c -} where cat2 :: a -> b -> c

instance Catenable2 a a (Vec2 a) where
  cat2 a b = Vec2 a b

instance Catenable2 a (Vec2 a) (Vec3 a) where
  cat2 a (Vec2 b c) = Vec3 a b c

instance Catenable2 a (Vec3 a) (Vec4 a) where
  cat2 a (Vec3 b c d) = Vec4 a b c d

instance Catenable2 (Vec2 a) a (Vec3 a) where
  cat2 (Vec2 a b) c = Vec3 a b c

instance Catenable2 (Vec2 a) (Vec2 a) (Vec4 a) where
  cat2 (Vec2 a b) (Vec2 c d) = Vec4 a b c d

instance Catenable2 (Vec3 a) a (Vec4 a) where
  cat2 (Vec3 a b c) d = Vec4 a b c d


class Catenable3 a b c d where cat3 :: a -> b -> c -> d

-- No fundeps in Catenable3, because I want scalar values in there, and
-- I'd rather not list all scalar choices.  If fundeps become important
-- (to reduce ambiguity), revisit.

