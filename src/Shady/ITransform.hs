{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies
           , UndecidableInstances
  #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.ITransform
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Invertible transformations
----------------------------------------------------------------------

module Shady.ITransform
  (
    ITransform(..), inverse, andInverse
  , ITrans(..)
  ) where

import Data.Monoid (Monoid(..))
import Control.Applicative (liftA2)

import Shady.Misc
import Shady.Complex (Complex)
import Shady.Language.Exp

-- | Transform with inverse.  The 'Monoid' instances is identity and
-- composition (in the usual order for composition of the forward
-- transformations).
data ITransform a = ITransform { itForward  :: Unop a
                               , itBackward :: Unop a }

instance Monoid (ITransform a) where
  mempty  = ITransform id id
  ITransform oF oB `mappend` ITransform iF iB =
    ITransform (oF.iF) (iB.oB)

-- | Inverse an 'ITransform'
inverse :: Unop (ITransform a)
inverse (ITransform forw back) = ITransform back forw

-- | Handy when we invert a transform by modifying the argument to the
-- transform's maker.
andInverse :: (c -> Unop a) -> (c -> c) -> c -> ITransform a
andInverse f inva = liftA2 ITransform f (f.inva)

infixr 1 *:

-- | Transformable values
class ITrans w a | a -> w where
  (*:) :: ITransform w -> Unop a

-- Inert on our basic types
instance ITrans w (E a) where
  (*:) = const id


-- Typical identity instance:

instance ITrans (Complex s) (Complex s) where (*:) = itForward

-- Distribute over tuple types.




instance (ITrans w a, ITrans w b) => ITrans w (a,b) where
  ix *: (a,b) = (ix *: a, ix *: b)

instance (ITrans w a, ITrans w b, ITrans w c) => ITrans w (a,b,c) where
  ix *: (a,b,c) = (ix *: a, ix *: b, ix *: c)

instance (ITrans w a, ITrans w b, ITrans w c, ITrans w d ) => ITrans w (a,b,c,d) where
  ix *: (a,b,c,d) =
    (ix *: a, ix *: b, ix *: c, ix *: d)


-- Function is an interesting case.  Inversely transform input.

instance (ITrans w a, ITrans w b) => ITrans w (a -> b) where
  ix *: f = (*:) ix . f . (*:) (inverse ix)


-- type Trans = forall a. ITrans w a => a -> a
