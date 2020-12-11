{-# Language OverloadedStrings #-}
import Advent
import Data.List (intercalate)
import Data.List.Split (splitOn)

type Bag = String
type Rule = (Bag, [(Int, Bag)])

bag :: Parser Bag
bag = some letterChar <> " " <> some letterChar <* " bag" <* optional "s"

bags :: Parser (Int, Bag)
bags = (,) <$> decimal <* " " <*> bag

contents :: Parser [(Int, Bag)]
contents = [] <$ "no other bags" <|> bags `sepBy1` ", "

rule :: Parser Rule
rule = (,) <$> bag <* " contain " <*> contents <* "."

canContain :: [Rule] -> Bag -> Bag -> Bool
canContain bags innerColor outerColor = go (lookup outerColor bags) where
  go (Just inner) = any ((==innerColor) . snd) inner ||
                    any ((canContain bags innerColor) . snd) inner

solve1 :: [Rule] -> Int
solve1 bags = length $ filter ((canContain bags "shiny gold") . fst) bags

countBags :: Bag -> [Rule] -> Int
countBags bagColor bags = go (lookup bagColor bags) where
  go (Just inner) = sum $ map (\(cnt, clr) -> cnt * (1 + countBags clr bags) ) inner

solve2 :: [Rule] -> Int
solve2 bags = (countBags "shiny gold" bags)

main :: IO ()
main = execute 7 (parseLines rule) [
  solve1 ,
  solve2  ]