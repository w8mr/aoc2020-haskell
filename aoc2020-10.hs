import Advent
import Data.List (sort, group)

diffs :: (Num a, Ord a) => [a] -> [a]
diffs input = zipWith (-) (tail full) full where
  sorted = sort input
  full = 0 : sorted ++ [3 + last sorted]

solve1 :: [Integer] -> Int
solve1 input = count (==1) d * count (==3) d where d = diffs input

solve2 :: [Integer] -> Int
solve2 input = (product . map (tribonacci . length) . filter ((==1) . head) . group . diffs) input where
  tribonacci x = [1,2,4,7] !! (x-1)

main :: IO ()
main = execute 10 (parseLines number) [
  solve1 ,
  solve2 ]