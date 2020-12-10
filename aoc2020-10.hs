{-# Language OverloadedStrings #-}
import Advent
import Data.List (sort)

solve1 :: [Integer] -> Integer
solve1 xs = go (sort (0:xs)) 0 0 where
  go (x:[]) c1 c3 = c1 * (c3+1)
  go (x:xs) c1 c3 | (x+1) `elem` xs = go xs (c1 + 1) c3
                  | (x+2) `elem` xs = go xs c1 c3
                  | (x+3) `elem` xs = go xs c1 (c3 + 1)


solve2 :: [Integer] -> Integer
solve2 xs = (product . (map (go . sort)) . brk . sort) (0:xs) where
  go (x:[]) = 1
  go (x:xs) = p c where
    p n = sum [go (drop (i-1) xs) | i <- [1..n]]
    c = sum [fromEnum $ (x+i) `elem` (take 3 xs) | i <- [1..3]]
  brk = go [] [] where
    go a1 a2 (x:[]) = ((x:a2):a1)
    go a1 a2 (x:y:xs) | x+3 == y  = go ((x:a2):a1) [x] (y:xs)
                      | otherwise = go a1 (x:a2) (y:xs)

main :: IO ()
main = execute 10 (parseLines number) [
  solve1 ,
  solve2  ]