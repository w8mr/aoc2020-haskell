{-# Language OverloadedStrings #-}
import Advent
import Data.List (nub, sort, tails, inits)

sums xs = [x + y | (x:ys) <- tails xs, y <-ys]

solve1 = go 25 where
  go n xs | length xs <= n = -1
          | x `elem` s     = go n (tail xs)
          | otherwise      = x where
    (preamble, xxs) = splitAt n xs
    s = sums preamble
    x = head xxs

solve2 xs = go (solve1 xs) xs where
  go n xs = if r == n then s else go n (tail xs) where
    initsSums = [ (sum ys, minimum ys + maximum ys) | ys <- inits xs ]
    (r,s) = (last . takeWhile ((<= n) . fst)) initsSums


main :: IO ()
main = execute 9 (parseLines number) [
  solve1 ,
  solve2  ]