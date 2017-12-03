module Day3 where

import Data.List as List

data Dir = LEFT | RIGHT | UP | DOWN deriving (Show, Eq)

-- move RIGHT means: I've already gone right, what to do now?
move :: Dir -> Int -> Int -> Int -> Int -> [Dir] -> [Dir]
move dir range moved count targetNum actionlist
  | targetNum == count = reverse actionlist
  | isCube count && range == moved = move dir (range +1) (moved + 1) (count + 1) targetNum (dir:actionlist) -- continue go dir --start new cycle with increased range
  | range == moved = move nextDir range 1 (count + 1) targetNum (nextDir:actionlist) -- change dir
  | otherwise = move dir range (moved + 1) (count + 1) targetNum (dir:actionlist) -- continue go dir
  where
    nextDir = nextDirection dir

reduceActions :: [Dir] -> [Dir]
reduceActions [] = []
reduceActions [dir] = [dir]
reduceActions (dir:dirlist)
  |  (opposite dir) `elem` dirlist = reduceActions $ List.delete (opposite dir) dirlist
  | otherwise = dir:reduceActions dirlist

opposite :: Dir -> Dir
opposite RIGHT = LEFT
opposite LEFT = RIGHT
opposite UP = DOWN
opposite DOWN = UP

nextDirection :: Dir -> Dir
nextDirection LEFT = DOWN
nextDirection RIGHT = UP
nextDirection UP = LEFT
nextDirection DOWN = RIGHT

computeSize :: Int -> Int
computeSize number = length $ reduceActions $ move RIGHT 1 0 1 number []

isCube x = x == head (dropWhile (< x) squares)
  where squares = scanl1 (+) [1,3..]
