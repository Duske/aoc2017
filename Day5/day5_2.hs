module Day5 where

import Data.Sequence

main :: IO ()
main = do
  input <- readFile "input.txt"
  let maze = map (\x -> read x :: Int ) $ lines input
  let mazeSeq = fromList maze
  putStr $ show $ doMaze2 mazeSeq (Prelude.length maze) 0 0

-- mazeLimit = upper limit
-- position = position at mazelist
doMaze2 :: Seq Int -> Int -> Int -> Int -> Int
doMaze2 maze mazeLimit position steps
  | position >= mazeLimit || position < 0 = steps
  | otherwise = doMaze2 (calcNewMaze maze (index maze position) position) mazeLimit nextPosition (steps + 1)
  where
    nextPosition = position + index maze position

calcNewMaze mazeSeq offset position
  | offset >= 3 = update (-1) position mazeSeq
  | otherwise = update (-1) position mazeSeq
