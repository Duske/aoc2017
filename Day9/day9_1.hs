module Day9_1 where


main :: IO ()
main = do
  input <- readFile "input.txt"
  putStr $ show $ calc input 0 0 False

calc :: String -> Int -> Int -> Bool -> Int
calc ('{':rest) level result garbage = if garbage then calc rest level result garbage else calc rest (level + 1) result garbage
calc ('}':rest) level result garbage = if garbage then calc rest level result garbage else calc rest (level - 1) (result + level) garbage
calc ('<':rest) level result garbage = if garbage then calc rest level result garbage else calc rest level result True
calc ('>':rest) level result _ =  calc rest level result False
calc ('!':_:rest) level result garbage =  calc rest level result garbage
calc (_:rest) level result garbage =  calc rest level result garbage
calc [] _ result _  = result
