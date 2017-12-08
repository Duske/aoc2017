module Day7_1 where

import Data.List
import Data.List.Split

data Program = Program String [String] deriving (Show)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let programs = map makeLineToProgram $ lines input
  let deps = flatten $ dependencyList programs
  putStr $ show $ compute programs deps

compute :: [Program] -> String -> Program
compute programs dependencies = head $ filter (\(Program name _) -> not $ name `isInfixOf` dependencies) programs

flatten :: [String] -> String
flatten = intercalate []

dependencyList :: [Program] -> [String]
dependencyList programs = foldl (\acc (Program _ list) -> acc ++ list) [] programs


makeLineToProgram :: String -> Program
makeLineToProgram line = Program name list
  where
    name = head $ splitOn " " line
    list = getList line

getList :: String -> [String]
getList string
  | "->" `isInfixOf` string = tail $ splitOn "->" string
  | otherwise = []
