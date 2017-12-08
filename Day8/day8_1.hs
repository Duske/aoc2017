module Day8_1 where

import Data.Map
import Data.List

data Instruction = Instruction String String Int (Int -> Int) (Int -> Bool)
type RegisterList = Map String Instruction

main :: IO ()
main = do
  input <- readFile "input.txt"
  let instructions = (Prelude.map (createInstruction . words) . lines) input
  let registerDict = doInstructions instructions Data.Map.empty
  let registerDict2 = doInstructionsB instructions Data.Map.empty
  putStr $ show $ maxDict registerDict
  putStr $ show $ maximum $ Data.List.map maxDict registerDict2

maxDict :: RegisterList -> Int
maxDict dic = maximum $ Data.Map.map getVal dic

doInstructions :: [Instruction] -> RegisterList -> RegisterList
doInstructions flines regs = Data.List.foldl' doInstruction regs flines

doInstructionsB :: [Instruction] -> RegisterList -> [RegisterList]
doInstructionsB flines regs = Data.List.scanl doInstruction regs flines

doInstruction :: RegisterList -> Instruction -> RegisterList
doInstruction regs (Instruction key cmpReg _ updt cond)
  | cond cmpRegValue = Data.Map.insert key (Instruction key cmpReg (updt currentValue) updt cond) regs
  | otherwise = regs
  where
    cmpRegValue = maybe 0 getVal $ Data.Map.lookup cmpReg regs
    currentValue = maybe 0 getVal $ Data.Map.lookup key regs

-- For each line, create an Instruction containing the key, the regKey for the condition,
-- the value, the update statement and the condition for the regKey register
createInstruction :: [String] -> Instruction
createInstruction [key, action, value, _, cmpReg, op, cmpVal] = Instruction key cmpReg 0 updt cond
  where
    cond = createCondition op (read cmpVal :: Int)
    updt = createUpdt action (read value :: Int)
createInstruction _ = error "Invalid inst"

getVal :: Instruction -> Int
getVal (Instruction _ _ value _ _) = value

createUpdt :: String -> Int -> Int -> Int
createUpdt "inc" value val = val + value
createUpdt "dec" value val = val - value
createUpdt _ _ _ = error "Invalid Statement"

createCondition :: String -> Int -> Int -> Bool
createCondition ">" cmpVal = flip (>) cmpVal
createCondition "<" cmpVal = flip (<) cmpVal
createCondition "==" cmpVal = (==) cmpVal
createCondition "!=" cmpVal = (/=) cmpVal
createCondition "<=" cmpVal = flip (<=) cmpVal
createCondition ">=" cmpVal = flip (>=) cmpVal
createCondition _ _ = error "Invalid condition"
