{-# Language OverloadedStrings #-}
import Advent
import Text.Megaparsec (someTill, lookAhead)
import Data.List (nub)

data Rule = Letter Int Char | Number Int [[Int]] deriving (Eq, Show)

fRule :: Parser Rule
fRule = (try fLetterRule) <|> (try fNumberRule)
fNumberList :: Parser [Int]
fNumberList = (decimal <* (optional " ")) `someTill` ("| " <|> (lookAhead "\n"))
fNumberRule :: Parser Rule
fNumberRule = Number <$> decimal <* ": " <*> many fNumberList <* "\n"
fLetterRule :: Parser Rule
fLetterRule = Letter <$> decimal <* ": \"" <*> letterChar <* "\"\n"

format :: Parser ([Rule],[String])
format = (,) <$> fRule `manyTill` "\n" <*> many (letterChar `manyTill` "\n")

findRule :: Int -> [Rule] -> Maybe Rule
findRule n (rule@(Letter i _):rules) | n == i = Just rule
findRule n (rule@(Number i _):rules) | n == i = Just rule
findRule _ (_:[]) = Nothing
findRule n (_:rules) = findRule n rules


replaceRule :: Rule -> [Rule] -> [Rule]
replaceRule replace rules = go rules [] where
  idx (Letter i _) = i
  idx (Number i _) = i
  i' = (idx replace)
  go [] acc = acc
  go (rule:rules) acc | idx rule == i'   = go rules (replace:acc)
  go (rule:rules) acc                    = go rules (rule:acc)

parseRule :: [Rule] -> Int -> [String]
parseRule rules n = go (findRule n rules) where
  go Nothing = []
  go (Just (Letter _ c)) = ([c]:[])
  go (Just (Number _ numbers)) = concatMap ( (map concat) . mult . map ((parseRule rules))) numbers

mult :: [[a]] -> [[a]]
mult (xs:[]) = [ [x] | x<- xs ]
mult (xs:xss) = [ (x:ys) | x <- xs, ys <- mult xss ]

solve (rules, messages) = count (`elem` messages) $ parseRule rules 0

solve1 = solve
solve2 = solve1

main :: IO ()
main = execute 19 (parseInput format) [
  solve1 ,
  solve2  ]