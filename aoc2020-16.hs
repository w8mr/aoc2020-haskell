{-# Language OverloadedStrings #-}
import Advent
import Data.List (union, intersect, sort, sortBy, groupBy, (\\))
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Text.Megaparsec.Char (spaceChar)
import Data.Maybe (mapMaybe)

--class: 1-3 or 5-7
--row: 6-11 or 33-44
--seat: 13-40 or 45-50
--
--your ticket:
--7,1,14
--
--nearby tickets:
--7,3,47
--40,4,50
--55,2,20
--38,6,12

type Rule = (String,Int,Int,Int,Int)
type Ticket = [Int]

fRule :: Parser Rule
fRule = (,,,,) <$> many (letterChar <|> spaceChar) <* ": " <*> decimal <* "-" <*> decimal <* " or " <*> decimal <* "-" <*> decimal <* "\n"

fRules :: Parser [Rule]
fRules = fRule `manyTill` "\n"

fTicket :: Parser Ticket
fTicket = decimal `sepBy` "," <* "\n"

format :: Parser ([Rule], Ticket, [Ticket])
format = (,,) <$> fRules <* "your ticket:\n" <*> fTicket  <* "\nnearby tickets:\n" <*> many fTicket

invalid :: Int -> Rule -> Bool
invalid field (name,l1,h1,l2,h2) = (field < l1 || h1 < field) && (field < l2 || h2 < field)

allInvalid :: [Rule] -> Int -> Bool
allInvalid rules field = all (invalid field) rules

solve1 :: ([Rule], Ticket, [Ticket]) -> Int
solve1 (rules, your, nearby) = sum $ concatMap (mapMaybe (\field -> if allInvalid rules field then Just field else Nothing )) nearby

--solve2 :: ([Rule], Ticket, [Ticket]) -> Int
solve2 (rules, your, nearby) = product yourValues where
  allValidTickets = filter (not . any (allInvalid rules)) (your:nearby)
  invalidFieldsByTicket = map (map (\field -> mapMaybe (\rule@(name,_,_,_,_) -> if invalid field rule then Just name else Nothing) rules )) allValidTickets
  invalidFieldsByIndex = foldl (\acc ticket -> map (uncurry union) $ zip acc ticket ) (replicate (length invalidFieldsByTicket) []) invalidFieldsByTicket
  ruleNames = map (\(name,_,_,_,_) -> name) rules
  validFieldsByIndex = sortBy (comparing (length.snd)) $ zip [0..19] $ map ((\\) ruleNames) invalidFieldsByIndex
  fieldByIndex = snd $ foldl (\(used, found) (idx, pos) -> let avail = head $ pos \\ used in ((avail:used), ((idx, avail):found) )) ([], []) validFieldsByIndex
  departureFields = filter ((=="departure") . head . (splitOn " ") . snd) fieldByIndex
  yourValues = map (\x -> your !! (fst x)) departureFields


main :: IO ()
main = execute 16 (parseInput format) [
  solve1 ,
  solve2  ]