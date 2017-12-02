module Day2 where

import qualified Text.Regex as R

main :: IO ()
main = do
  input <- readFile "input.txt"
  let linesOfFiles = lines input
  let rows = map makeLineToRow linesOfFiles
  putStr $ show $ computeCheckSum rows

computeCheckSum :: [[Int]] -> Int
computeCheckSum rows = sum $ map computeCheckSumForRow rows

computeCheckSumForRow :: [Int] -> Int
computeCheckSumForRow row = maximum row - minimum row

makeLineToRow :: String -> [Int]
makeLineToRow string = map (\x -> read x :: Int ) stringRow
  where stringRow =  R.splitRegex (R.mkRegex "\t") string
