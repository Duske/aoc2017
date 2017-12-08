-- create dic from first string before " "
-- parse actions
-- parse condition = create a function which accepts a dic, set the value like "f" in there

module Day8_1 where

import Data.Map
import Data.Maybe

data Instruction = Instruction String Int (Int -> Int) (RegisterList -> Bool)
type RegisterList = Map String Instruction

main :: IO ()
main = do
  input <- readFile "input.txt"
  let linesofRawInstruction = Prelude.map words $ lines input
  let instructions = Prelude.map createInstruction linesofRawInstruction
  let dic = Data.Map.fromList instructions
  let newDic = compute dic
  putStr $ show $  maxDict newDic

maxDict :: RegisterList -> Int
maxDict dic = maximum $ Data.Map.map getVal dic

compute :: RegisterList -> RegisterList
compute dic = Data.Map.foldl' func dic dic
  where
    func dicAcc (Instruction key value stmt cond)  = if cond dicAcc then Data.Map.adjust (const (Instruction key (stmt value) stmt cond)) key dicAcc else dicAcc
    --newInstruction op (Instruction key value stmt cond) = Instruction key (op value) stmt cond

createInstruction :: [String] -> (String, Instruction)
createInstruction [key, action, value, _, dicKey, cmp, cmpVal] = (key, Instruction key 0 stmt cond)
  where
    cond = createCondition dicKey cmp (read cmpVal :: Int)
    stmt = createStmt action (read value :: Int)
createInstruction _ = error "Invalid inst"

getVal :: Instruction -> Int
getVal (Instruction _ value _ _) = value

createStmt :: String -> Int -> Int -> Int
createStmt "inc" value val = val + value
createStmt "dec" value val = val - value
createStmt _ _ _ = error "Invalid Statement"

createCondition :: String -> String -> Int -> RegisterList -> Bool
createCondition dicKey ">" cmpVal dictionary = (getVal . fromJust . Data.Map.lookup dicKey) dictionary > cmpVal
createCondition dicKey "<" cmpVal dictionary = (getVal . fromJust . Data.Map.lookup dicKey) dictionary < cmpVal
createCondition dicKey "==" cmpVal dictionary = (getVal . fromJust . Data.Map.lookup dicKey) dictionary == cmpVal
createCondition dicKey "!=" cmpVal dictionary = (getVal . fromJust . Data.Map.lookup dicKey) dictionary /= cmpVal
createCondition dicKey "<=" cmpVal dictionary = (getVal . fromJust . Data.Map.lookup dicKey) dictionary <= cmpVal
createCondition dicKey ">=" cmpVal dictionary = (getVal . fromJust . Data.Map.lookup dicKey) dictionary >= cmpVal
createCondition _ _ _ _ = error "Invalid condition"
