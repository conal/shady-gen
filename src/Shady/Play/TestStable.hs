-- Testing stable name generation

import System.Mem.StableName

test :: a -> IO Int
test a = fmap hashStableName (makeStableName a)

test' :: a -> IO ()
test' a = test a >>= print

main1 = mapM test [1..10]

main2 = sequence [test 1, test 2, test 1, test 3]

-- Doesn't reuse 1.  My guess: two different fromInteger calls.
main3 = test' 1 >> test' 2 >> test' 1 >> test' 3

-- This one really reuses 1.
main4 = test' o >> test' 2 >> test' o >> test' 3
 where
   o = 1 :: Int

-- Will ghc -O reuse the fromInteger 1?  Yes!
main = main3
