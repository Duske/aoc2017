module Day1_2 where

import Data.Char

computeCaptcha :: String -> Int -> Int
computeCaptcha input index
  | length input == index = 0 --abort
  | val == input!!possibleMatchIndex = digitToInt val + computeCaptcha input (index + 1) -- match on calc.position
  | otherwise = computeCaptcha input (index +1) --no match
  where
    possibleMatchIndex = calcNextIndex input index
    val = input!!index

-- calculate index of the next possible matching value
calcNextIndex :: String -> Int -> Int
calcNextIndex string index = mod (searchOffset + index) ln
  where
    ln = length string
    searchOffset = div (length string) 2

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStr $ show $ computeCaptcha input 0
