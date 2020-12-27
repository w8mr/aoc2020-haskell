{-# Language OverloadedStrings #-}
import Advent
import Text.Megaparsec (someTill, lookAhead)
import Data.List (find, sortBy, minimumBy, maximumBy)
import Data.Map (fromList,insert, assocs)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (comparing)

type IndexedRule = (Int, Rule)
type LoebArray = [String -> [Maybe String]]
data Rule = Letter Char | Number [[Int]] deriving (Eq, Show)

fRule :: Parser IndexedRule
fRule = (,) <$> decimal <* ": " <*> (fLetterRule <|> fNumberRule)
fNumberList :: Parser [Int]
fNumberList = (decimal <* (optional " ")) `someTill` ("| " <|> (lookAhead "\n"))
fNumberRule :: Parser Rule
fNumberRule = Number <$> many fNumberList <* "\n"
fLetterRule :: Parser Rule
fLetterRule = Letter <$ "\"" <*> letterChar <* "\"\n"

format :: Parser ([IndexedRule],[String])
format = (,) <$> fRule `manyTill` "\n" <*> (many letterChar) `sepBy` "\n"

loeb :: Functor f => f (f a -> a) -> f a
loeb = moeb fmap

-- | 'loeb' generalized over 'fmap'
moeb :: (((a -> b) -> b) -> c -> a) -> c -> a
moeb f = \x -> let go = f ($ go) x in go

matchLetter :: Char -> LoebArray -> String -> [Maybe String]
matchLetter c _ [] = [Nothing]
matchLetter c _ (x:xs) = [if x == c then Just xs else Nothing]

matchAltSubRules :: [[Int]] -> LoebArray -> String -> [Maybe String]
matchAltSubRules [] _ xs = [Just xs]
matchAltSubRules nns rules xs = concatMap (matchSubRules rules xs) nns where
  matchSubRules :: LoebArray -> String -> [Int] -> [Maybe String]
  matchSubRules _ xs [] = [Just xs]
  matchSubRules rules xs (n:ns) = concatMap evaluate $ matchRule rules xs n where
    matchRule rules xs n = (rules !! n) xs
    evaluate (Just ys) = matchSubRules rules ys ns
    evaluate Nothing = [Nothing]

match :: LoebArray -> Int -> String -> Bool
match rules n xs = any isMatch $ matchAltSubRules [[n]] rules xs where
  isMatch (Just "") = True
  isMatch _ = False

solve :: ([IndexedRule],[String]) -> Int
solve (rules, messages) = count (match (loeb loebArray) 0) messages where
  loebArray = [la rule | idx <- [0..(maximum $ map fst rules)], let rule = lookup idx rules ] where
    la (Just (Letter c)) = matchLetter c
    la (Just (Number nums)) = matchAltSubRules nums
    la Nothing = matchAltSubRules []

solve1 (rules, messages) = solve (rules, messages)
solve2 (rules, messages) = solve (map replace rules, messages) where
  replace rule@(idx, _) = fromMaybe rule $ find ((==idx) . fst) [
    (8, (Number [[42], [42, 8]])),
    (11, (Number [[42, 31], [42, 11, 31]]))
    ]

main :: IO ()
main = execute 19 (parseInput format) [
  solve1 ,
  solve2  ]