module Day1 where

import Data.Char

computeDigit :: String -> Int
computeDigit [] = 0
computeDigit [x] = computeDigitFromString [x,x]
computeDigit (x:xs) = computeDigitFromString (x:xs ++ [x])

computeDigitFromString :: String -> Int
computeDigitFromString [] = 0
computeDigitFromString [_] = 0

computeDigitFromString [x,y]
  | x == y = digitToInt x + computeDigitFromString [y]
  | otherwise = computeDigitFromString [y]

computeDigitFromString (x:y:xs)
  | x == y = digitToInt x + computeDigitFromString (y:xs)
  | otherwise = computeDigitFromString (y:xs)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStr $ show $ computeDigit input
