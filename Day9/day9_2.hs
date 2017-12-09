module Day9_1 where


main :: IO ()
main = do
  input <- readFile "input.txt"
  putStr $ show $ calc input False

calc :: String -> Bool -> Int
calc ('!':_:rest) garbage =  calc rest garbage -- the skip operator needs to match with highest priority
calc ('>':rest) _ =  calc rest False -- end garbage "mode" before garbage catchall
calc (_:rest) True = 1 + calc rest True -- catch all chars while in garbage "mode" and plus one
calc ('<':rest) False = calc rest True -- start garbage "mode" when not active
calc (_:rest) False =  calc rest False -- catch non-operator chars like 'a,b,c...' in non-garbage "mode"
calc [] _  = 0 -- return result
