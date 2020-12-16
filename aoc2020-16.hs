{-# Language OverloadedStrings #-}
import Advent
import Data.List (union, intersect, sort, sortBy, groupBy, (\\))
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Text.Megaparsec.Char (spaceChar)

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

type Field = (String,Int,Int,Int,Int)
type Ticket = [Int]

fField :: Parser Field
fField = (,,,,) <$> many (letterChar <|> spaceChar) <* ": " <*> decimal <* "-" <*> decimal <* " or " <*> decimal <* "-" <*> decimal <* "\n"

fFields :: Parser [Field]
fFields = fField `manyTill` "\n"

fTicket :: Parser Ticket
fTicket = decimal `sepBy` "," <* "\n"

format :: Parser ([Field], Ticket, [Ticket])
format = (,,) <$> fFields <* "your ticket:\n" <*> fTicket  <* "\nnearby tickets:\n" <*> many fTicket

allowedInField (name, l1,h1,l2,h2) = concat [[l1..h1],[l2..h2]]
allowedInFields fields = foldl1 union ((map allowedInField) fields)

invalidNumbers allowed tickets = filter (not . ((flip elem) allowed)) tickets
solve1 (fields, your, nearby) = sum $ concatMap (invalidNumbers (allowedInFields fields)) nearby

valid allowed tickets = filter (all ((flip elem) allowed)) tickets

possibleFieldIndexes f_i field tickets = concat $ filter ((>0). length) $ map (\(t_i, t) -> map (\x -> (f_i,fst x)) $ filter (\(i,n) -> not $ n `elem` (allowedInField field)) (zip [1..] t)) (zip [1..] tickets)

solve2 :: ([Field], Ticket, [Ticket]) -> Int
solve2 (fields, your, nearby) = product s where
  s = map (\x -> your !! ((snd x)-1)) q
  q= filter ((=="departure") . head . (splitOn " ") . fst) i
  i = snd $ foldl (\(used, found) (name, pos) -> let avail = head $ pos \\ used in ((avail:used), ((name, avail):found) )) ([], []) validPositions
  validPositions = sortBy (comparing (length . snd)) $ map (\(name, pos) -> (name, [1..20] \\ pos)) invalidPositions
  invalidPositions = map (\x -> (fst $ head x, sort $ map snd x)) $ groupBy grp invalidFields
  invalidFields = concat $ map (\(f_i, field@(name,_,_,_,_)) -> possibleFieldIndexes name field validTickets) (zip [1..] fields)
  validTickets = valid (allowedInFields fields) (nearby ++ [your])

sortGT (a1, c1) (a2, c2)
  | a1 < a2 = LT
  | a1 > a2 = GT
  | a1 == a2 = compare c1 c2

grp (a1, c1) (a2, c2) = a1 == a2


main :: IO ()
main = execute 16 (parseInput format) [
  solve1 ,
  solve2  ]