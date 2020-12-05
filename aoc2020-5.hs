import Advent
import Data.List (sort)

convert :: [Char] -> Int
convert = go 64 4 0 0 where
  go :: Int -> Int -> Int -> Int -> [Char] -> Int
  go _ _  c r      []  = r * 8 + c
  go h h2 c r ('L':xs) = go h (h2 `div` 2) c r xs
  go h h2 c r ('R':xs) = go h (h2 `div` 2) (c+h2) r xs
  go h h2 c r ('F':xs) = go (h `div` 2) h2 c r xs
  go h h2 c r ('B':xs) = go (h `div` 2) h2 c (r+h) xs

findMissing (x:y:xs) | (x + 1) /= y = x + 1
                     | otherwise    = findMissing (y:xs)

solve1 = maximum . map convert
solve2 = findMissing . sort . map convert

main :: IO ()
main = execute 5 (lines) [
  solve1 ,
  solve2  ]