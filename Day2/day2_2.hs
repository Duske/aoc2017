module Day2_2 where

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
computeCheckSumForRow row = head [div x y | x <- row, y <- row, x `mod` y == 0 , x > y ]

makeLineToRow :: String -> [Int]
makeLineToRow string = map (\x -> read x :: Int ) stringRow
  where stringRow =  R.splitRegex (R.mkRegex "\t") string
