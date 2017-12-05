module Day5 where

main :: IO ()
main = do
  input <- readFile "input.txt"
  let maze = map (\x -> read x :: Int ) $ lines input
  putStr $ show $ doMaze maze (length maze) 0 0

-- mazeLimit = upper limit
-- position = position at mazelist
doMaze :: [Int] -> Int -> Int -> Int -> Int
doMaze maze mazeLimit position steps
  | position >= mazeLimit || position < 0 = steps
  | otherwise = doMaze newMaze mazeLimit nextPosition (steps + 1)
  where
    nextPosition = position + maze!!position
    newMaze = replaceAtIndex position ((maze!!position) + 1) maze

-- util function to update list at position n
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, _ : b) = splitAt n ls
