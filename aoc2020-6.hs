{-# Language OverloadedStrings #-}
import Advent
import Data.List (union, intersect)

format :: Parser [[[Char]]]
format = (some letterChar `endBy` "\n") `sepBy` "\n"

-- Map over all groups. Fold over answers of persons with union/intersect
-- Count combined answers. Sum over groups
solve f = sum . map (length . foldl1 f)
solve1 = solve union
solve2 = solve intersect

main :: IO ()
main = execute 6 (parseInput format) [
  solve1,
  solve2 ]