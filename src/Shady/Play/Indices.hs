-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Play.Indices
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPL-3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Playing with index generation for surfaces
----------------------------------------------------------------------

module Shady.Play.Indices where

rows,cols :: Int
rows = 3
cols = 3

nats :: [Int]
nats = [0 ..]

is,js :: [Int]
is = take rows nats
js = take cols nats

-- Construct a single tri-strip built from rows.  The even rows (starting
-- with zero) go from left to right, with each index pair being
-- (below,above).  The odd rows go from right to left, (above,below).
-- Each row transition produces a degenerate triangle, which OpenGL discards.

-- Indices for ith horizontal strip.
row :: Int -> [(Int,Int)]
row i = (flat.f) [ ((i,j),(i+1,j)) | j <- js ]
 where
   f | even i    = map (\ (a,b) -> (b,a))
     | otherwise = reverse
   flat          = concat . map (\ (x,y) -> [x,y])

ijs :: [(Int,Int)]
ijs = concat (map row is)

indices' :: [Int]
indices' = map (\ (i,j) -> i * cols + j) ijs
