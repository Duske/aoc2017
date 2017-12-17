module Day12 where

import Data.Map
import Data.Maybe
import Data.List.Split

type Connection = (Int, [Int])

main :: IO ()
main = do
  input <- readFile "input.txt"
  let connections = (Prelude.map parseInput . lines) input
  let initDict = compute connections (Data.Map.singleton 0 0)
  let lastDict = repeatUntilStable initDict connections
  putStr $ show $ Data.Map.size lastDict

repeatUntilStable :: Map Int Int -> [Connection] -> Map Int Int
repeatUntilStable dict connections = if Data.Map.size newDict > Data.Map.size dict then repeatUntilStable newDict connections else dict
  where
    newDict = compute connections dict

compute :: [Connection] -> Map Int Int -> Map Int Int
compute connections dict = Prelude.foldl check dict connections
  where
    check acc (num, list) = if isJust (lookupIndex num acc) then addList acc list else acc

addList :: Map Int Int -> [Int] -> Map Int Int
addList dict list = Prelude.foldl (\acc item -> insert item item acc) dict list

parseInput :: String -> Connection
parseInput line = (read start ::Int, connectList list)
  where
    [start, list] = splitOn " <-> " line
    connectList string = Prelude.map (\val -> read val ::Int) $ splitOn ", " string
