import Advent
import Data.List (tails, inits, find)

solve1 = go 25 where
  go n xs | x `elem` sums = go n (tail xs)
          | otherwise     = x where
    (preamble, (x:_)) = splitAt n xs
    sums = [x + y | (x:ys) <- tails preamble, y <-ys]

solve2 xs = go (solve1 xs) xs where
  go n xs | r == n    = s
          | otherwise = go n (tail xs) where
    initsSums = [ (sum ys, minimum ys + maximum ys) | ys <- inits xs ]
    Just (r,s) = find ((>= n) . fst) initsSums

main :: IO ()
main = execute 9 (parseLines number) [
  solve1 ,
  solve2  ]