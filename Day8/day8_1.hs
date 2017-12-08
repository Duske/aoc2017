-- create dic from first string before " "
-- parse actions
-- parse condition = create a function which accepts a dic, set the value like "f" in there

module Day8_1 where

import Data.Map
import Data.Maybe

data Instruction = Instruction String Int (Int -> Int) (Map String Instruction -> Bool)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let linesofRawInstruction = Prelude.map words $ lines input
  let instructions = Prelude.map createInstruction linesofRawInstruction
  let dic = Data.Map.fromList instructions
  let newDic = compute dic
  putStr $ show $  maxDic newDic

maxDic dic = Data.Map.foldl check 0 dic
  where
    check acc (Instruction _ value _ _ ) = if acc > value then acc else value

compute :: Map String Instruction -> Map String Instruction
compute dic = Data.Map.foldl func dic dic
  where
    func dicAcc (Instruction key _ stmt cond)  = if cond dicAcc then Data.Map.adjust (newInstruction stmt) key dicAcc else dicAcc
    newInstruction op (Instruction key value stmt cond) = Instruction key (op value) stmt cond

createInstruction :: [String] -> (String, Instruction)
createInstruction [key, action, value, _, dicKey, cmp, cmpVal] = (key, Instruction key 0 stmt cond)
  where
    cond = createCondition dicKey cmp (read cmpVal :: Int)
    stmt = createStmt action (read value :: Int)
createInstruction _ = ("", Instruction "" 0 id (const False))

getVal :: Instruction -> Int
getVal (Instruction _ value _ _) = value

createStmt :: String -> Int -> Int -> Int
createStmt "inc" value val = val + value
createStmt "dec" value val = val - value
createStmt _ _ _ = 0

createCondition :: String -> String -> Int -> Map String Instruction -> Bool
createCondition dicKey ">" cmpVal dictionary = (getVal . fromJust . Data.Map.lookup dicKey) dictionary > cmpVal
createCondition dicKey "<" cmpVal dictionary = (getVal . fromJust . Data.Map.lookup dicKey) dictionary < cmpVal
createCondition dicKey "==" cmpVal dictionary = (getVal . fromJust . Data.Map.lookup dicKey) dictionary == cmpVal
createCondition dicKey "!=" cmpVal dictionary = (getVal . fromJust . Data.Map.lookup dicKey) dictionary /= cmpVal
createCondition dicKey "<=" cmpVal dictionary = (getVal . fromJust . Data.Map.lookup dicKey) dictionary <= cmpVal
createCondition dicKey ">=" cmpVal dictionary = (getVal . fromJust . Data.Map.lookup dicKey) dictionary >= cmpVal
createCondition _ _ _ _ = False
