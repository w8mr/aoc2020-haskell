import Advent
import Data.List (intercalate)
import Data.List.Split (splitOn)

parseLine line = (bag, contents) where
  (bag:contents:[]) = splitOn " bags contain " line

parseContents = splitOn ", " . init

parseBag bag = (if amount == "no" then 0 else read amount :: Int, intercalate " " color) where
  (amount:color) = (init . words) bag

parse line = (bag, inner) where
  (bag, contents) = parseLine line
  inner = map parseBag $ parseContents contents

canContain :: [(String, [(Int, String)])] -> String -> String -> Bool
canContain bags innerColor outerColor = go (lookup outerColor bags) where
  go Nothing = False
  go (Just inner) = if any ((==innerColor) . snd) inner
                               then True
                               else any ((canContain bags innerColor). snd) inner

solve1 :: [([Char], [(Int, [Char])])] -> Int
solve1 bags = length $ filter ((canContain bags "shiny gold") . fst) bags

countBags :: String -> [(String, [(Int, String)])] -> Int
countBags bagColor bags = go (lookup bagColor bags) where
  go Nothing = 0
  go (Just inner) = 1 + (sum $ map (\(cnt, clr) -> cnt * countBags clr bags ) inner)

solve2 :: [([Char], [(Int, [Char])])] -> Int
solve2 bags = (countBags "shiny gold" bags) - 1

main :: IO ()
main = execute 7 (map parse . lines) [
  solve1 ,
  solve2  ]