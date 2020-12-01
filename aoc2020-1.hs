import Advent
import Data.Char(digitToInt)

solve :: [Int] -> Int
solve = solve' . pairsHalf where
  solve' ((x, y):xs) | x + y == 2020 = x * y
  solve' ((x, y):xs) | otherwise     = solve' xs

solve2 :: [Int] -> Int
solve2 = solve2' . triplesHalf where
  solve2' ((x, y, z):xs) | x + y + z == 2020 = x * y * z
  solve2' ((x, y, z):xs) | otherwise         = solve2' xs

main :: IO ()
main = execute 1 readIntLines [
  solve,
  solve2 ]