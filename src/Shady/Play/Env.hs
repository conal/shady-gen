{-# LANGUAGE GADTs, KindSignatures, TypeOperators, TypeFamilies
           , Rank2Types, PatternGuards, ScopedTypeVariables
           , StandaloneDeriving, GeneralizedNewtypeDeriving
           , MultiParamTypeClasses, FlexibleContexts, FlexibleInstances
           , TypeSynonymInstances
  #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Play.Env
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPL-3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Playing with expressions-in-environments.
-- 
----------------------------------------------------------------------


module Play.Env where

import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad

-- experiment
import Unsafe.Coerce

import Graphics.Shady.Language.Equality

infixr 5 :+


{--------------------------------------------------------------------
    Strays
--------------------------------------------------------------------}

deriving instance Functor     First
deriving instance Applicative First
deriving instance Monad       First

fromFirst :: a -> First a -> a
fromFirst a = fromMaybe a . getFirst


{--------------------------------------------------------------------
    Variables
--------------------------------------------------------------------}

data (a :+ v) x where
  ZeroV :: (a :+ v) a
  SuccV :: {- SynEq v => -} v b -> (a :+ v) b

instance SynEq v => SynEq (a :+ v) where
  ZeroV   =-= ZeroV    = True
  SuccV v =-= SuccV v' = v =-= v'
  _       =-= _        = False

-- | Tweak the inside of each 'Succ'
onSucc :: (f :--> g) -> (a :+ f :--> a :+ g)
onSucc _ ZeroV     = ZeroV
onSucc f (SuccV v) = SuccV (f v)


{--------------------------------------------------------------------
    Application (not used)
--------------------------------------------------------------------}

infixl 4 :<*>

data App :: ((* -> *) -> (* -> *)) where
  (:<*>) :: h (a -> b) -> h a -> App h b


infixr 3 :-->

type f :--> g = forall a . f a -> g a

mapP :: (f :--> g) -> (App f :--> App g)
mapP h (f :<*> a) = h f :<*> h a

data E :: (* -> *) -> * -> * where
  Lit :: a -> E v a
  Var :: v a -> E v a
  -- App :: App (E v) b -> E v b
  (:^) :: E v (a -> b) -> E v a -> E v b
  Lam :: E (a :+ v) b -> E v (a -> b)

down1 :: E v b -> E (a :+ v) b
down1 = undefined

instance Functor (E v) where fmap f = (pure f <*>)
instance Applicative (E v) where
  pure  = Lit
  (<*>) = (:^)   -- or smart variant

-- Value environment

class HasEnv v where
  type Env v
  vval :: v a -> Env v -> a

instance HasEnv v => HasEnv (a :+ v) where
  type Env (a :+ v)   = (a, Env v)
  vval ZeroV     (a,_) = a
  vval (SuccV v) (_,e) = vval v e

eval :: HasEnv v => E v x -> Env v -> x
eval (Lit  x) = pure x
eval (Var  v) = vval v
eval (f :^ a) = eval f <*> eval a
eval (Lam  b) = flip (curry (eval b))

-- For Lam,
-- 
--     b :: E (a :+ v) b
--     eval b :: Env (a :+ v) -> b
--               (a, Env v) -> b
--     curry (eval b) :: a -> Env v -> b
--     flip (curry (eval b)) :: Env v -> a -> b

-- Could make an Eval class.

pval :: HasEnv v => App (E v) x -> Env v -> x
pval (f :<*> a) = eval f <*> eval a

-- Pull the eval out with mapP?  requires Id x as result.

-- In composition style, using App:
-- 
--     eval (Lit  x) = pure x
--     eval (Var  v) = vval v
--     eval (App  p) = pval p
--     eval (Lam  b) = (flip . curry . eval) b
-- 
--     eval . Lit = pure
--     eval . Var = vval
--     eval . App = pval
--     eval . Lam = flip . curry . eval


-- | Map polymorphic function over an expression 
mapE :: (u :--> v) -> E u :--> E v
mapE = undefined


{--------------------------------------------------------------------
    Concatenation
--------------------------------------------------------------------}

infixr 5 :++

-- type family (:++) (u :: * -> *) :: (* -> *) -> (* -> *)

-- Concatenation of variable types

type family (u :: * -> *) :++ (v :: * -> *) :: * -> *

type None = Const ()

type instance None :++ v = v

type instance (a :+ u) :++ v = a :+ (u :++ v)

-- type instance (:++) (a :+ u) = a :+ (u :++ v)

-- Given ls :: Lams l, I know that SynEq v => SynEq (l :++ v).  Proof is
-- by induction on ls.


-- Proof that SynEq (l :++ v)
data CatEq l v where
  CatEq :: SynEq (l :++ v) => CatEq l v

foo :: Lams l -> forall v. SynEq v => CatEq l v
foo _ = unsafeCoerce ()

-- I'm stumped on how to define construct these proofs, though I know
-- they're valid.  The following gives an error, perhaps having to do with
-- lack of known surjectivity of :++.  Revisit.

-- foo LamZ     = CatEq
-- foo (LamS r) = case foo r of CatEq -> CatEq

--     Could not deduce (SynEq (v2 :++ v1))
--       from the context (SynEq (v2 :++ v))
--       arising from a use of `CatEq' at Play/Env.hs:179:25-29



-- l == a :+ l'
-- r :: Lams l'

-- foo r :: CatEq l' v

-- have:

--   CatEq :: SynEq (l' :++ v) => CatEq l' v

-- want:

--   CatEq :: SynEq (a :+ l' :++ v) => CatEq (a :+ l') v


-- data CatEq l where
--   CatEq :: forall v. SynEq (l :++ v) => CatEq l

-- foo :: Lams l -> CatEq l
-- foo LamZ     = CatEq
-- foo (LamS r) = case foo r of CatEq -> CatEq



{--------------------------------------------------------------------
    Another angle on expression environments
--------------------------------------------------------------------}

-- | Bindings for variables in v', back to v.

infixr 5 :>, <++>

-- | Let-binding environment
data Lets :: (* -> *) -> (* -> *) -> * where
  LetZ :: Lets v v
  (:>) :: E v' a -> Lets v' v -> Lets (a :+ v') v

pureL :: E v a -> Lets (a :+ v) v
pureL a = a :> LetZ

-- | Append binding environments
(<++>) :: Lets v'' v' -> Lets v' v -> Lets v'' v
LetZ     <++> s = s
(a :> r) <++> s = a :> (r <++> s)

-- | Bump variables past given environment.
bumpLV :: Lets v' v -> v a -> v' a
bumpLV LetZ     v = v
bumpLV (_ :> r) v = SuccV (bumpLV r v)

-- | Bump variables past given environment.
bumpL :: Lets v' v -> E v :--> E v'
bumpL l = mapE (bumpLV l)

-- | Nested @let@ expressions.
wrapLets :: Lets v' v -> E v' :--> E v
wrapLets LetZ e = e
wrapLets (a :> r) e = wrapLets r (letE a e)

letE :: E v a -> E (a :+ v) b -> E v b
letE a f = Lam f :^ a
           -- lamE f ^: a


{--------------------------------------------------------------------
    Lambda contexts
--------------------------------------------------------------------}

-- Intervening lambdas
data Lams v where
  LamZ :: Lams None
  LamS :: Lams v -> Lams (a :+ v)

-- | Bump variables past given lambda stack
bumpLamV :: Lams l -> v :--> l :++ v
bumpLamV LamZ     v = v
bumpLamV (LamS r) v = SuccV (bumpLamV r v)

-- | Transform variables within some lambdas
pastLamsV :: Lams l -> (u :--> v) -> (l :++ u :--> l :++ v)

pastLamsV LamZ     = id
pastLamsV (LamS r) = \ u -> f u where f = onSucc . pastLamsV r

-- Also okay, but less efficient for partial application:
-- 
--     pastLamsV LamZ     u = u
--     pastLamsV (LamS r) u = (onSucc . pastLamsV r) u

-- However, "Inferred type is less polymorphic than expected":
-- 
--     pastLamsV LamZ     = id
--     pastLamsV (LamS r) = onSucc (pastLamsV r)


-- | Bump variables past given environment.
bumpL' :: Lams l -> Lets v' v -> E (l :++ v) :--> E (l :++ v')
bumpL' lams lets = mapE (pastLamsV lams (bumpLV lets))


{--------------------------------------------------------------------
    Possibly-failing transformations
--------------------------------------------------------------------}

infixr 3 :->?, :-->?

type a :->?  b = a -> First b

type f :-->? g = forall a. f a :->? g a

mapE' :: (u :-->? v) -> (E u :-->? E v)
mapE' = undefined

predV' :: a :+ v :-->? v
predV' ZeroV     = mempty
predV' (SuccV v) = pure v

predVs' :: Lams l -> (l :++ v) :-->? v
predVs' LamZ      = pure
predVs' (LamS l') = predVs' l' <=< predV'

-- TODO: fold on Lams?

extract :: Lams l -> E (l :++ v) :-->? E v
extract l = mapE' (predVs' l)


{--------------------------------------------------------------------
    CSE
--------------------------------------------------------------------}

trivial :: E v a -> Bool
trivial = undefined

find :: E v p -> E v :-->? E (p :+ v)
find = undefined


-- | CSE results.  l holds the lamda bindings on the way to p.
data CSE l v p q =
  forall v'. CSE (Lets v' v) (E (l :++ v') p) (E v' q)

cse :: forall l v p q. 
       Lams l -> E (l :++ v) p -> E v q -> CSE l v p q

-- cse l p q | trivial p           = noCse p q
--           | Just q' <- find l p q =
--               CSE (pureL p) (Var ZeroV) q'

cse l p q | trivial p             = noCse p q
          | First (Just p') <- extract l p
          , First (Just q') <- find p' q  =
              let z = ZeroV :: (p :+ v) p in
                 CSE (pureL p') (Var (bumpLamV l z)) q'
          | otherwise             = decomp l p q

-- cse _ p q = noCse p q

decomp :: Lams l -> E (l :++ v) p -> E v q -> CSE l v p q

decomp l (r :^ s) q =
  case cse l r q of
    CSE letsU r' q' ->
      case cse l (bumpL' l letsU s) q' of
        CSE letsV s' q'' ->
          CSE (letsV <++> letsU) (bumpL' l letsV r' :^ s') q''

decomp l (Lam u) q =
  case cse (LamS l) u q of
    CSE letsU u' q' ->
      CSE letsU (Lam u') q'

decomp _ p q = noCse p q

-- noCse :: Lams l -> E (l :++ v) p -> E v q -> CSE l v p q
-- noCse _ p q = CSE LetZ p q

noCse     :: E (l :++ v) p -> E v q -> CSE l v p q
noCse p q = CSE LetZ p q

{-

-}
