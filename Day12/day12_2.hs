module Day12 where

import Data.Map
import Data.Maybe
import Data.List.Split

type Connection = (Int, [Int])

main :: IO ()
main = do
  input <- readFile "input.txt"
  let connections = (Prelude.map parseInput . lines) input
  putStr $ show $ repeatUntilGroupIsStable connections

compInit :: [(a,b)] -> a
compInit = fst . head

-- for each stable group remove it from connections and start again
repeatUntilGroupIsStable :: [Connection] -> Int
repeatUntilGroupIsStable [] = 0
repeatUntilGroupIsStable [_] = 1
repeatUntilGroupIsStable connection = 1 + repeatUntilGroupIsStable newCon
  where
    lastDict = repeatUntilStable (Data.Map.singleton initValue initValue) connection
    initValue = compInit connection
    newCon = removeConnections connection lastDict

-- remove all connections which are in the dict
removeConnections :: [Connection] -> Map Int Int -> [Connection]
removeConnections connections dict = Prelude.filter check connections
  where
    check (val, _) = isNothing (lookupIndex val dict)

-- repeat adding connections to dict until size of dict does not change -> stable
repeatUntilStable :: Map Int Int -> [Connection] -> Map Int Int
repeatUntilStable dict connections = if newSize > oldSize then repeatUntilStable newDict connections else dict
  where
    newDict = compute connections dict
    newSize = Data.Map.size newDict
    oldSize = Data.Map.size dict

-- add connection list to dict if connection item is already in dict
-- or: a <-> b,c,d : add b,c,d if a is already in dict
compute :: [Connection] -> Map Int Int -> Map Int Int
compute connections dict = Prelude.foldl check dict connections
  where
    check acc (num, list) = if isJust (lookupIndex num acc) then addList acc list else acc


addList :: Map Int Int -> [Int] -> Map Int Int
addList dict list = Prelude.foldl (\acc item -> insert item item acc) dict list

-- make each line to a Connection type
parseInput :: String -> Connection
parseInput line = (read start ::Int, connectList list)
  where
    [start, list] = splitOn " <-> " line
    connectList string = Prelude.map (\val -> read val ::Int) $ splitOn ", " string
