import Advent
import Data.List (group, sort, nub)
import Data.List.Split (splitOn)

split :: String -> [[String]]
split = (map (filter ((>0) . length) . splitOn "\n") . splitOn "\n\n")

solve1 = (sum . map (length . nub. sort . concat))
solve2 = (sum .map (\(c, a) -> (count id . map (==c) . map length . group .sort) a) . map ((\x -> (length x, concat x))))

main :: IO ()
main = execute 6 (split) [
  solve1,
  solve2 ]