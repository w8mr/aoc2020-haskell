import Advent
import Data.Char(digitToInt)
import Data.String (lines)

solve :: [Int] -> Int
solve = solve_i . pairsHalf

solve_i ((x, y):xs) = if x+y == 2020 then x*y else solve_i xs

solve2 :: [Int] -> Int
solve2 = solve2_i . triplesHalf

solve2_i ((x, y, z):xs) = if x+y+z == 2020 then x*y*z else solve2_i xs


main :: IO ()
main = execute 1 readIntLines [
  solve,
  solve2 ]