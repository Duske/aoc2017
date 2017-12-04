module Day4 where
import Data.List

main :: IO ()
main = do
  input <- readFile "input.txt"
  let linesOfPassphrases = map (map sort . words) $ lines input
  let validPassphrases = filter isUnique linesOfPassphrases
  putStr $ show $  length validPassphrases

isUnique :: (Eq a) => [a] -> Bool
isUnique []     = True
isUnique (x:xs) = x `notElem` xs && isUnique xs
