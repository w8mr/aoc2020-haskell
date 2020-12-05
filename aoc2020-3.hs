import Advent

solve :: [(Int, Int)] -> [[Char]] -> Int
solve slopes input = product $ map (go 0 0 0) slopes where
  rowLength = length $ head input
  go _ ind_r count _ | ind_r >= length input = count
  go ind_c ind_r count s = go new_ind_c new_ind_r new_count s where
    inc '#' = 1
    inc _   = 0
    cell = (input !! ind_r) !! ind_c
    new_ind_c = (ind_c + fst s) `mod` rowLength
    new_ind_r = ind_r + snd s
    new_count = count + inc cell

solve1 = solve [(3,1)]
solve2 = solve [(1,1), (3,1), (5,1), (7,1), (1,2)]

main :: IO ()
main = execute 3 (lines) [
  solve1 ,
  solve2  ]