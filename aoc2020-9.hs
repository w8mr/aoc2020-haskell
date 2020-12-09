import Advent
import Data.List (tails, inits)

solve1 = go 25 where
  go n xs | length xs <= n = -1
          | x `elem` s     = go n (tail xs)
          | otherwise      = x where
    (preamble, (x:_)) = splitAt n xs
    sums xs = [x + y | (x:ys) <- tails xs, y <-ys]
    s = sums preamble

solve2 xs = go (solve1 xs) xs where
  go n xs | r == n    = s
          | otherwise = go n (tail xs) where
    initsSums = [ (sum ys, minimum ys + maximum ys) | ys <- inits xs ]
    (r,s) = (last . takeWhile ((<= n) . fst)) initsSums

main :: IO ()
main = execute 9 (parseLines number) [
  solve1 ,
  solve2  ]