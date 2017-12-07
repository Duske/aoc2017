module Day6 where

import Data.List as List
import qualified Text.Regex as R

main :: IO ()
main = do
  input <- readFile "input.txt"
  let banks = makeLineToBanks input
  putStr $ show $ compute banks []

compute :: [Int] -> [[Int]] -> Int
compute list memory
  | list `elem` memory = 0
  | otherwise = 1 + compute (distribute listToDistribute count start) (list:memory)
  where
    count = maximum list
    listToDistribute = replaceAtIndex start 0 list
    start = maxIdx list

distribute :: [Int] -> Int -> Int -> [Int]
distribute list 0 _ = list
distribute list count startIdx = distribute newList (count - 1) nextIdx
  where
    newList = replaceAtIndex nextIdx (list!!nextIdx + 1) list
    nextIdx = (startIdx + 1) `mod` length list

makeLineToBanks :: String -> [Int]
makeLineToBanks string = map (\x -> read x :: Int ) stringRow
  where stringRow =  R.splitRegex (R.mkRegex "\t") string

-- util function to update list at position n
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, _ : b) = List.splitAt n ls

maxIdx :: [Int] -> Int
maxIdx list = head $ filter ((== maximum list) . (list !!)) [0..]
