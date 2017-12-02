module Day2 where

import qualified Text.Regex as R

main :: IO ()
main = do
  input <- readFile "input.txt"
  let linesOfFiles = lines input
  let rows = map makeRowToItems linesOfFiles
  putStr $ show $ computeCheckSum rows


computeCheckSum :: [[String]] -> Integer
computeCheckSum rows = sum $ map computeCheckSumForRow rows

computeCheckSumForRow :: [String] -> Integer
computeCheckSumForRow row =
  let intRow = map (\x -> read x :: Integer ) row
  in maximum intRow - minimum intRow

makeRowToItems :: String -> [String]
makeRowToItems string = R.splitRegex (R.mkRegex "\t") string
