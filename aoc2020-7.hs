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


canContainShinyGold :: [(String, [(Int, String)])] -> String -> Bool
canContainShinyGold allBags bagColor = go (lookup bagColor allBags) where
  go :: Maybe [(Int, String)] -> Bool
  go Nothing = False
  go (Just []) = False
  go (Just inner) = if any ((=="shiny gold") . snd) inner
                               then True
                               else any ((canContainShinyGold allBags). snd) inner

solve1 :: [([Char], [(Int, [Char])])] -> Int
solve1 bags = length $ filter ((canContainShinyGold bags) . fst) bags



countBags :: String -> [(String, [(Int, String)])] -> Int
countBags bagColor allBags = go (lookup bagColor allBags) where
  go :: Maybe [(Int, String)] -> Int
  go Nothing = 0
  go (Just []) = 0
  go (Just inner) = 1 + (sum $ map (\(cnt, clr) -> cnt * countBags clr allBags ) inner)

solve2 :: [([Char], [(Int, [Char])])] -> Int
solve2 bags = (countBags "shiny gold" bags) - 1

main :: IO ()
main = execute 7 (map parse . lines) [
  solve1 ,
  solve2  ]