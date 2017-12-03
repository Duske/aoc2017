module Day3 where

import Data.List as List

data Dir = LEFT | RIGHT | UP | DOWN deriving (Show, Eq)

-- move RIGHT means: I've already gone right, what to do now?
move :: Dir -> Int -> Int -> Int -> Int -> [Dir] -> [Dir]
move RIGHT range moved count targetNum actionlist
  | targetNum == count = reverse actionlist
  | isCube count && range == moved = move RIGHT (range +1) (moved + 1) (count + 1) targetNum (RIGHT:actionlist) -- continue go right --start new cycle with increased range
  | range == moved = move UP range 1 (count + 1) targetNum (UP:actionlist) -- change dir
  | otherwise = move RIGHT range (moved + 1) (count + 1) targetNum (RIGHT:actionlist) -- continue go right

move UP range moved count targetNum actionlist
  | targetNum == count = reverse actionlist
  | isCube count && range == moved = move UP (range +1) (moved + 1) (count + 1) targetNum (UP:actionlist) -- continue go up --start new cycle with increased range
  | range == moved = move LEFT range 1 (count + 1) targetNum (LEFT:actionlist) -- change dir
  | otherwise = move UP range (moved + 1) (count + 1) targetNum (UP:actionlist) -- continue go up

move LEFT range moved count targetNum actionlist
  | targetNum == count = reverse actionlist
  | isCube count && range == moved = move LEFT (range +1) (moved + 1) (count + 1) targetNum (LEFT:actionlist) -- continue go left --start new cycle with increased range
  | range == moved = move DOWN range 1 (count + 1) targetNum (DOWN:actionlist) -- change dir WITH INCREASED RANGE!
  | otherwise = move LEFT range (moved + 1) (count + 1) targetNum (LEFT:actionlist) -- continue go left

move DOWN range moved count targetNum actionlist
  | targetNum == count = reverse actionlist
  | isCube count && range == moved = move DOWN (range +1) (moved + 1) (count + 1) targetNum (DOWN:actionlist) -- continue go down --start new cycle with increased range
  | range == moved = move RIGHT range 1 (count + 1) targetNum (RIGHT:actionlist) -- change dir
  | otherwise = move DOWN range (moved + 1) (count + 1) targetNum (DOWN:actionlist) -- continue go down

reduceActions :: [Dir] -> [Dir]
reduceActions [] = []
reduceActions [dir] = [dir]
reduceActions (LEFT:dirlist)
  |  RIGHT `elem` dirlist = reduceActions $ List.delete RIGHT dirlist
  | otherwise = LEFT:reduceActions dirlist
reduceActions (RIGHT:dirlist)
  |  LEFT `elem` dirlist = reduceActions $ List.delete LEFT dirlist
  | otherwise = RIGHT:reduceActions dirlist
reduceActions (DOWN:dirlist)
  | UP `elem` dirlist = reduceActions $ List.delete UP dirlist
  | otherwise = DOWN:reduceActions dirlist
reduceActions (UP:dirlist)
  | DOWN `elem` dirlist = reduceActions $ List.delete DOWN dirlist
  | otherwise = UP:reduceActions dirlist

computeSize :: Int -> Int
computeSize number = length $ reduceActions $ move RIGHT 1 0 1 number []

isCube x = x == head (dropWhile (< x) squares)
  where squares = scanl1 (+) [1,3..]
