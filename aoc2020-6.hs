import Advent
import Data.List (group, sort, nub)
import Data.List.Split (splitOn)

split :: String -> [[[Char]]]
split = (map (filter ((>0) . length) . splitOn "\n") . splitOn "\n\n")

-- Map over all groups, within the group take all letters,
-- deduplicate(nub), get the length. Sum over all lengths
solve1 = sum . map (length . nub . concat)

-- Map over all groups, make tuple with number of people and all letters)
-- Map over tuples. sort and group the letters.
-- Get Length of groups, count lengths which are same as number of people. Sum count
solve2 = sum . map (\(c, a) -> (count (==c) . map length . group .sort) a) . map ((\x -> (length x, concat x)))

main :: IO ()
main = execute 6 (split) [
  solve1,
  solve2 ]