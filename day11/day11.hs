module Day11 where

import Data.List.Split

type Vector = (Int, Int, Int)

data Dir = N | S | NW | NE | SW | SE deriving Show

-- move from one hexfield to another
-- we move by sliding on two axises to reach a neighbor field
move :: Vector -> Dir -> Vector
move (x,y,z) N = (x, y + 1, z - 1)
move (x,y,z) S = (x, y - 1, z + 1)
move (x,y,z) NE = (x + 1, y, z -1)
move (x,y,z) SW = (x - 1, y, z + 1)
move (x,y,z) NW = (x - 1, y + 1, z)
move (x,y,z) SE = (x + 1, y -1, z)

toDir :: String -> Dir
toDir ('n':'e':_) = NE
toDir "se" = SE
toDir "sw" = SW
toDir "nw" = NW
toDir "s" = S
toDir "n" = N
toDir f = error f

-- in hex, we need to divide the steps by 2, since we can go directly to a
-- neighbor field, but we always calculate 2 steps
distance :: Vector -> Int
distance (x,y,z) = div (abs(x) + abs(y) + abs(z)) 2

main :: IO ()
main = do
  input <- readFile "input.txt"
  let dirs = map toDir $ splitOn "," input
  let endVector = foldl move (0,0,0) dirs
  let vectorList = scanl move (0,0,0) dirs
  putStr $ show $ distance endVector
  putStr $ show $ maximum $ map distance vectorList
  --putStr $ show $ calc input 0 0 False
