import Advent
import Data.List (tails)

solve1 :: [Int] -> Int
solve1 xs = head [ x * y | (x:ys) <- tails xs, y <- ys, x + y == 2020]

solve2 :: [Int] -> Int
solve2 xs = head [ x * y * z | (x:ys) <- tails xs, (y:zs) <- tails ys, z <- zs, x + y + z == 2020]

main :: IO ()
main = execute 1 (parseLines number) [
  (solve1),
  (solve2)]
