import Advent
import Data.List (group, sort, nub, union, intersect)
import Data.List.Split (splitOn)

split :: String -> [[[Char]]]
split = (map (filter ((>0) . length) . splitOn "\n") . splitOn "\n\n")

-- Map over all groups. Fold over answers of persons with union/intersect
-- Count combined answers. Sum over groups
solve f = sum . map (length . foldl1 f)
solve1 = solve union
solve2 = solve intersect

main :: IO ()
main = execute 6 (split) [
  solve1,
  solve2 ]