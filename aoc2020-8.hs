{-# Language OverloadedStrings #-}
import Advent

type Instruction = (String, Int)
format :: Parser Instruction
format = (,) <$> many letterChar <* " " <*> number <* eof

solve input = go ([], 0, 0) where
  go :: ([Int], Int, Int) -> (Int, Bool)
  go (hit, acc, ip) | elem ip hit = (acc, True)
                    | ip >= (length input) = (acc, False)
                    | otherwise = go ((ip:hit), acc', ip') where
    process ("nop", param) = (acc        , ip + 1)
    process ("acc", param) = (acc + param, ip + 1)
    process ("jmp", param) = (acc        , ip + param)
    (acc', ip') = process (input !! ip)

solve1 = fst . solve

changeInstruction :: [Instruction] -> Int -> [Instruction]
changeInstruction input ip = xs ++ [new] ++ ys where
  (xs,((instr,param):ys)) = splitAt ip input
  new = (change instr, param)
  change "nop" = "jmp"
  change "jmp" = "nop"
  change instr = instr

solve2 input = go 0 where
  go cip = let r = solve $ changeInstruction input cip in
             if snd r then go (cip + 1) else fst r

main :: IO ()
main = execute 8 (parseLines format) [
  solve1 ,
  solve2  ]