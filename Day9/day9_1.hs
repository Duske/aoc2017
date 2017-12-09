module Day9_1 where


main :: IO ()
main = do
  input <- readFile "input.txt"
  putStr $ show $ calc input 0 0 False

calc :: String -> Int -> Int -> Bool -> Int
calc ('!':_:rest) level result garbage =  calc rest level result garbage -- the skip operator needs to fetch with highest priority
calc ('>':rest) level result _ =  calc rest level result False -- end garbage operator before garbage catchall
calc (_:rest) level result True = calc rest level result True -- catch all chars while in garbage "mode"
calc ('{':rest) level result False = calc rest (level + 1) result False  -- increase level
calc ('}':rest) level result False = calc rest (level - 1) (result + level) False -- decrease level but increase the result
calc ('<':rest) level result False = calc rest level result True -- start garbage "mode" when not active
calc (_:rest) level result False =  calc rest level result False -- catch non-operator chars like 'a,b,c...'
calc [] _ result _  = result -- return result
