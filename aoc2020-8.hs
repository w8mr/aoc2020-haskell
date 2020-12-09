{-# Language OverloadedStrings #-}
import Advent

data Param = Direct Int | Relative Int deriving (Show, Eq)
data Instruction = Nop Param | Acc Param | Jmp Param deriving (Show, Eq)
data RunState = Normal | Halt | Loop deriving (Show, Eq)
data Registers = Registers { acc :: Int} deriving (Show, Eq)
data State = State { usedIp :: [Int], regs :: Registers, ip :: Int, runState :: RunState} deriving (Show, Eq)

format, fNop, fAcc, fJmp  :: Parser Instruction
fNop = Nop . Direct <$ "nop " <*> number <* eof
fAcc = Acc . Direct <$ "acc " <*> number <* eof
fJmp = Jmp . Relative <$ "jmp " <*> number <* eof
format = fNop <|> fAcc <|> fJmp

process :: State -> Instruction -> State
process state (Nop (Direct param))   = changeIpBy 1 state
process state (Acc (Direct param))   = changeIpBy 1 (changeAccBy param state)
process state (Jmp (Relative param)) = changeIpBy param state

initState :: State
initState = State [] (Registers 0) 0 Normal

updateRunState :: RunState -> State -> State
updateRunState new (State usedIp (Registers acc) ip runState) = State usedIp (Registers acc) ip new
loop = updateRunState Loop
halt = updateRunState Halt

updateUsedIp :: Int -> State -> State
updateUsedIp added (State usedIp (Registers acc) ip runState) = State (added:usedIp) (Registers acc) ip runState

changeAccBy :: Int -> State -> State
changeAccBy new (State usedIp (Registers acc) ip runState) = State usedIp (Registers (acc + new)) ip runState

changeIpBy :: Int -> State -> State
changeIpBy diff (State usedIp regs ip runState) = State usedIp regs (ip + diff) runState

solve :: [Instruction] -> State
solve instructions = go initState where
  go :: State -> State
  go state@(State usedIp (Registers acc) ip' runState)
    | ip' `elem` usedIp = loop state
    | ip' >= length instructions = halt state
    | otherwise = go (updateUsedIp ip' (process state (instructions !! (ip state))))

solve1 = acc . regs . solve

changeInstruction :: [Instruction] -> Int -> [Instruction]
changeInstruction input ip = xs ++ [new] ++ ys where
  (xs,(instr:ys)) = splitAt ip input
  new = change instr
  change (Nop (Direct param)) = Jmp (Relative param)
  change (Jmp (Relative param)) = Nop (Direct param)
  change instr = instr

solve2 input = go 0 where
  go :: Int -> Int
  go cip = go' (runState state) where
     state = solve $ changeInstruction input cip
     go' Halt = (acc . regs) state
     go' Loop = go (cip + 1)

main :: IO ()
main = execute 8 (parseLines format) [
  solve1,
  solve2 ]