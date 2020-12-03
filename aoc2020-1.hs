import Advent
import Data.Char(digitToInt)

--solve (x:xs) = if ((check x xs) > 0) then check x xs else solve (tail xs)
--
--check n (y:ys) = if n+y == 2020 then n*y else check n ys
--check _ [] = 0






--solve :: Int -> [Int] -> Int
--solve size = solve' . (subsequencesOfSize size) where
--  solve' (xxs:xs) | sum xxs == 2020 = product xxs
--  solve' (xxs:xs) | otherwise       = solve' xs

solve = check . pairsHalf

check ((x, y):xs) = if x + y == 2020 then x * y else check xs



solve1 :: [Int] -> Int
solve1 = solve' . pairsHalf where
  solve' ((x, y):xs) | x + y == 2020 = x * y
  solve' ((x, y):xs) | otherwise     = solve' xs

solve2 :: [Int] -> Int
solve2 = solve' . triplesHalf where
  solve' ((x, y, z):xs) | x + y + z == 2020 = x * y * z
  solve' ((x, y, z):xs) | otherwise         = solve' xs

main :: IO ()
main = execute 1 readIntLines [
  (solve),
  (solve2)]

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if n>l then [] else subsequencesBySize xs !! (l-n)
 where
   subsequencesBySize [] = [[[]]]
   subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                             in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])