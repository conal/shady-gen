{-# LANGUAGE Rank2Types #-}

memo :: (k -> v) -> (k -> v)
memo = undefined

type MFun f = forall a. Maybe a -> f a

memoM :: MFun f -> MFun f
memoM h = memo h

-- Why does memoM type-check?
--
-- If I eta-contract to memoM = memo, I get what'd expect for the
-- non-contracted def:
--
--     Couldn't match expected type `MFun f'
--            against inferred type `k -> v'

