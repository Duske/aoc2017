module Day15_2 where

import Data.Bits

stepA :: Int -> (Int, Bool)
stepA value = (possibleVal, possibleVal `mod` 4 == 0)
  where possibleVal = (value * 16807) `mod` 2147483647

stepB :: Int -> (Int, Bool)
stepB value = (possibleVal, possibleVal `mod` 8 == 0)
  where possibleVal = (value * 48271) `mod` 2147483647

check :: Int -> Int -> Bool
check valA valB = (.&.) 0xffff valA == (.&.) 0xffff valB


-- compute2 5000000 0 883 879
compute2 :: Int -> Int -> Int -> Int -> Int
compute2 0 count _ _ = count
compute2 steps count valA valB
  | noSkipA && noSkipB && check nextA nextB = compute2 (steps - 1) (count + 1) nextA nextB
  | noSkipA && noSkipB = compute2 (steps - 1) count nextA nextB
  | otherwise = compute2 steps count nextA nextB
  where
    (nextA, noSkipA) = stepA valA
    (nextB, noSkipB) = stepB valB
