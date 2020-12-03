import Advent

solve :: [(Int, Int)] -> [[Char]] -> Int
solve slopes input = product (map (go (length (head input)) 0 0 0 input) slopes) where
  go rl c r count input s | r >= (length input) = count
                          | otherwise = go rl ((c + fst s) `mod` rl) (r + snd s) (count + increment) input s where
    cell = (input !! r) !! c
    increment = if cell == '#' then 1 else 0

--solve :: [(Int, Int)] -> [[Char]] -> Int
--solve slopes input = go (length (head input)) 0 0 (cycle slopes) 0 input where
--  go _ _ r _ count input | r >= (length input) = count
--  go rowLength c r (s:ss) count input | (length input) == r = count
--                                      | otherwise = go rowLength ((c + (fst s)) `mod` rowLength) (r + (snd s)) ss (count + increment) input where
--    cell = (input !! r) !! c
--    increment = if cell == '#' then 1 else 0


solve1 = solve [(3,1)]
solve2 = solve [(1,1),(3,1),(5,1),(7,1),(1,2)]

main :: IO ()
main = execute 3 (lines) [
  solve1 ,
  solve2  ]