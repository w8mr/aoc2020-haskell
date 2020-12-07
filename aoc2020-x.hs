{-# Language OverloadedStrings #-}
import Advent

solve = id

solve1 = solve
solve2 = solve1

main :: IO ()
main = execute 15 (lines) [
  solve1 ,
  solve2  ]