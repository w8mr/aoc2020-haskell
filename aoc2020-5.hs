import Advent
import Data.List (sort)

convert :: [Char] -> Int
convert = go 0 where
  go :: Int -> [Char] -> Int
  go c       [] = c
  go c (x:xs) = go (2 * c + b x) xs where
    b 'R' = 1
    b 'B' = 1
    b 'L' = 0
    b 'F' = 0

findMissing (x:y:xs) | (x + 1) /= y = x + 1
                     | otherwise    = findMissing (y:xs)

solve1 = maximum . map convert
solve2 = findMissing . sort . map convert

main :: IO ()
main = execute 5 (lines) [
  solve1 ,
  solve2  ]