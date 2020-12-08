{-# Language OverloadedStrings #-}
import Advent

type Instruction = (String, Int)
format :: Parser Instruction
format = (,) <$> many letterChar <* " " <*> number <* eof


solve input = go ([], 0, 0) where
  go :: ([Int], Int, Int) -> (Int, Bool)
  go (hit, acc, ip) | elem ip hit = (acc, True)
                    | ip >= (length input) = (acc, False)
                    | otherwise = go2 (hit, acc, ip) (input !! ip)
  go2 (hit, acc, ip) ("nop", param) = go ((ip:hit), acc, ip + 1)
  go2 (hit, acc, ip) ("acc", param) = go ((ip:hit), acc+param, ip + 1)
  go2 (hit, acc, ip) ("jmp", param) = go ((ip:hit), acc, ip + param)

solve1 = fst . solve

changeInstruction :: [Instruction] -> Int -> [Instruction]
changeInstruction input ip = xs ++ [new] ++ ys where
  (xs,(old:ys)) = splitAt ip input
  new = c old
  c ("nop", param) = ("jmp", param)
  c ("jmp", param) = ("nop", param)
  c ins = ins


solve2 input = go 0 where
  go cip = let r = solve $ changeInstruction input cip in
             if snd r then go (cip + 1) else fst r

main :: IO ()
main = execute 8 (parseLines format) [
  solve1 ,
  solve2  ]