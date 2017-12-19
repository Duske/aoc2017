module Day15_1 where

import Data.Bits

stepA :: Int -> Int
stepA value = (value * 16807) `mod` 2147483647

stepB :: Int -> Int
stepB value = (value * 48271) `mod` 2147483647

check :: Int -> Int -> Bool
check valA valB = (.&.) 0xffff valA == (.&.) 0xffff valB

check2 :: Int -> Int -> Bool
check2 valA valB = valA `mod` 4 == valB `mod` 8


-- compute 40000000 0 883 879
compute :: Int -> Int -> Int -> Int -> Int
compute 0 count _ _ = count
compute steps count valA valB
  | check nextA nextB = compute (steps - 1) (count + 1) nextA nextB
  | otherwise = compute (steps - 1) count nextA nextB
  where
    nextA = stepA valA
    nextB = stepB valB

-- compute2 5000000 0 883 879
compute2 :: Int -> Int -> Int -> Int -> Int
compute2 0 count _ _ = count
compute2 steps count valA valB
  | check2 nextA nextB = compute2 (steps - 1) (count + 1) nextA nextB
  | otherwise = compute2 (steps - 1) count nextA nextB
  where
    nextA = stepA valA
    nextB = stepB valB
