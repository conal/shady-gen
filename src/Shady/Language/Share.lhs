 <!-- -*- markdown -*-

> {-# LANGUAGE GADTs, KindSignatures, Rank2Types, TypeOperators
>   , PatternGuards, NamedFieldPuns, StandaloneDeriving
>   , ScopedTypeVariables
>   #-}
> {-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

> {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP

|
Module      :  Shady.Language.Share
Copyright   :  (c) Conal Elliott 2010
License     :  BSD3
Maintainer  :  conal@conal.net
Stability   :  experimental

 -->

Experiments with sharing recovery on GADT-based expression representations.

> module Shady.Language.Share where

Imports
=======

> import Prelude hiding (foldr)
> 
> import Data.Function (on)
> import Data.Ord (comparing)
> import Data.Maybe (fromMaybe)
> import Data.List (sortBy)
> import Control.Applicative (Applicative(..),liftA2,(<$>))
> import Control.Arrow (first,second,(&&&))
> import Data.Foldable (foldr)
>
> import qualified Control.Monad.State as S
>
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Data.Set (Set)
> import qualified Data.Set as Set
>
> import Data.Proof.EQ

> import Shady.Language.Type
> import Shady.Language.Operator
> import Shady.Language.Exp


Misc orphans
============

(Unneeded. In Data.NameM)

 > instance Applicative (State s) where { pure = return ; (<*>) = ap }

Temporary names
===============

Some temporary names during conversion.

> type Exp = E
> type Name = Id

Free variables
==============

As often, it'll be handy to compute free variable sets:

> freeVars :: Exp a -> Set Name
> freeVars (Var (V n _))   = Set.singleton n
> freeVars (Op _)          = Set.empty
> freeVars (f :^ a)        = freeVars f `Set.union` freeVars a
> freeVars (Lam (V n _) b) = Set.delete n (freeVars b)


Graphs
======

A graph is a map from nodes to variable names, plus a root expression (typically a  variable).
A graph node is just an expression, but it will typically not be nested.

> type Graph a = (Exp a, Map TExp Name)

A `TExp` is like `T Exp`, but with an `Ord` instance for use with `Map`:

> data TExp = forall a. HasType a => TExp (Exp a)

> tFreeVars :: TExp -> Set Name
> tFreeVars (TExp e) = freeVars e

The reason for mapping from a node to index instead of vice versa is just that it's  more efficient to build in this direction.
We'll invert the map later when we convert back from `Graph` to `Exp`.

> invertMap :: Ord v => Map k v -> Map v k
> invertMap = Map.fromList . map (\ (n,i) -> (i,n)) . Map.toList

To use `TExp` as a map key, it'll have to be ordered.
For simplicity, I'll just use the printed form of the `Exp`.

> instance Eq TExp where
>   TExp p == TExp q = show p == show q
> 
> instance Ord TExp where
>   TExp p `compare` TExp q = show p `compare` show q

*TODO:* experiment with storing the `show` form in the `TExp`, to save recomputing it. Compare speed.


Common subexpression elimination
================================

To elimination common subexpressions, convert from expression to graph (dag) and back  to expression.
The final expression will use `let` (as beta redexes) to abstract out expressions  that appear more than once.

> cse :: HasType a => Exp a -> Exp a
> cse = undagify . dagify


Conversion from Exp to Graph (dag)
==================================

I'll structure conversion from `Exp` to Graph (dag) around a monad for computations  that accumulate 
an exp map and a list of unused names.
The names are guaranteed to be in ascending order so that we can trivially top-sort  the graph later.

> type ExpMap = Map TExp Name

> type GraphM = S.State (ExpMap, [Name])

> dagify :: HasType a => Exp a -> Graph a
> dagify e = second fst $ S.runState (dagifyExp e)
>                            (Map.empty, map reverse $ tail allNames)
>  where
>    allNames :: [Name]
>    allNames = "" : [c:name | name <- allNames, c <- ['a'..'z']]

The name list is not alphabetized and moreover could not be alpabetized.
Define a comparison function, which could be compare length/string pairs.

> compareNames :: String -> String -> Ordering
> compareNames = comparing (length &&& id)

Graph construction works by recursively constructing and inserting graph nodes:

> dagifyExp :: HasType a => Exp a -> GraphM (Exp a)
> dagifyExp e = dagN e >>= insertG
> 
> dagN :: HasType a => Exp a -> GraphM (Exp a)
> dagN (Var v)   = pure $ Var v
> dagN (Op o)    = pure $ Op o
> dagN (f :^ a)  = liftA2 (:^) (dagifyExp f) (dagifyExp a)
> -- dagN (Lam v b) = Lam v <$> dagifyExp b
> dagN (Lam _ _) = error "dagN: Can't yet perform CSE on Lam"

If the given node is already in the graph, reuse the existing node id.
Otherwise, insert insert it, giving it a new node id.

> insertG :: HasType a => Exp a -> GraphM (Exp a)
> insertG node = maybe (addExp node) return
>                 =<< findExp node <$> S.gets fst
> 
> addExp :: HasType a => Exp a -> GraphM (Exp a)
> addExp node = do name <- genName
>                  S.modify (first (Map.insert (TExp node) name))
>                  return (Var (var name))

Needing `HasType` in `insertG` forced me to add it several other places, including in  the `Exp` constructor types.

Identifier generation is as usual, accessing and incrementing the counter state:

> genName :: GraphM Name
> genName = do (m,name:names) <- S.get
>              S.put (m,names)
>              return name

To search for an exp in the accumulated node map,

> findExp :: HasType a => Exp a -> ExpMap -> Maybe (Exp a)
> findExp n = fmap (Var . var) . Map.lookup (TExp n)

Conversion from Graph (dag) to Exp
==================================

Given a `Graph`, let's now build an `Exp`, with sharing.

Recall the `Graph` type and map inversion, defined above:

< type Graph a = (Exp a, Map TExp Name)

To rebuild an `Exp`, walk through the inverted map in order, generating a `let` for  each node.

< undagify :: forall a. HasType a => Graph a -> Exp a
< undagify (root,nodeToName) =
<   foldr bind root (sortedBinds (invertMap nodeToName))
<  where
<    bind :: (Name,TExp) -> Exp a -> Exp a
<    bind (name, TExp rhs) = lett name rhs

> sortedBinds :: Map Name TExp -> [(Name,TExp)]
> sortedBinds = sortBy (compareNames `on` fst) . Map.toList


Inlining
--------

To minimize the `let` bindings, let's re-inline all bindings that are used only once.
To know how which bindings are used only once, count them.

> inlinables :: Map Name TExp -> Set Name
> inlinables m = asSet $ (== 1) <$> countUses m

< countUses :: Map Name TExp -> Map Name Int
< countUses = foldr addBind Map.empty . Map.elems
<  where
<    addBind :: TExp -> Map Name Int -> Map Name Int
<    addBind te m = foldr (\ fv -> Map.insertWith (+) fv 1) m (tFreeVars te)

This definition of `countUses` is more complicated than I'd like.
Try again, this time extracting all of the free variables in all of the expressions,  and then counting.

> countUses :: Map Name TExp -> Map Name Int
> countUses = histogram . concatMap (Set.toList . tFreeVars) . Map.elems

Count the number of occurrences of each member of a list:

> histogram :: Ord a => [a] -> Map a Int
> histogram = foldr incMap Map.empty

Increment a map element:

> incMap :: (Ord k, Num n) => k -> Map k n -> Map k n
> incMap k = Map.insertWith (+) k 1

Turn a boolean map (characteristic function) into a set:

> asSet :: Ord k => Map k Bool -> Set k
> asSet = Set.fromList . Map.keys . Map.filter id

Now revisit `undagify`, performing some inlining along the way.

> undagify :: forall a. HasType a => Graph a -> Exp a
> undagify (root,nodeToName) = foldr bind (inline root) (sortedBinds texps)
>  where
>    texps :: Map Name TExp
>    texps = invertMap nodeToName
>    ins :: Set Name
>    ins = inlinables texps
>    bind :: (Name,TExp) -> Exp a -> Exp a
>    bind (name, TExp rhs) = lett' name (inline rhs)
>    -- Inline texps in an expression
>    inline :: Exp b -> Exp b
>    inline (Var v@(V name _)) | Set.member name ins = tLookup v texps
>    inline (f :^ a) = inline f :^ inline a
>    inline (Lam v b) = Lam v (inline b) -- assumes no shadowing
>    inline e = e
>    -- Make a let binding unless an inlined variable.
>    lett' :: (HasType b, HasType c) =>
>             Name -> Exp b -> Exp c -> Exp c
>    lett' n rhs | Set.member n ins = id
>                | otherwise        = letE (var n) rhs

For the inlining step, we'll have to look up a variable in the map, and check that it  has the required type.

> tLookup :: V a -> Map Name TExp -> Exp a
> tLookup (V name tya) m =
>   fromTExp tya (fromMaybe (error "undagify: tLookup failure")
>                           (Map.lookup name m))

> fromTExp :: Type a -> TExp -> Exp a
> fromTExp tya (TExp e) | Just Refl <- typeOf1 e `tyEq` tya = e
>                       | otherwise = error "fromTExp type fail"

I'm not satisfied having to deal the type check explicitly here.
Maybe a different abstraction would help; perhaps a type-safe homogeneous map instead  of `Map Name TExp`.


  <!-- References -->

 [semantic editor combinator]:  http://conal.net/blog/posts/semantic-editor-combinators/ "blog post"
