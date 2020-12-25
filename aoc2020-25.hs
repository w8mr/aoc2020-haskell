{-# Language OverloadedStrings #-}
import Advent
import Data.List (find)
import Data.Maybe (fromJust)

formula subject (x,i) = (x*subject `mod` 20201227, succ i)
formulaIter n = iterate (formula n) (1,0)
findLoopSize pub = snd $ fromJust $ find ((==pub) . fst) $ formulaIter 7
powerMod base exp = fst $ head $ drop exp $ formulaIter base

factor :: Int -> [Int]
factor 1 = []
factor n = let prime = head $ dropWhile ((/= 0) . mod n) [2 .. n]
           in (prime :) $ factor $ div n prime

solve input = foldl (\acc exp -> (powerMod acc exp) `mod` 20201227) pubKeyDoor $ factor loopSizeCard where
  (pubKeyCard:pubKeyDoor:_) = input
  loopSizeCard = findLoopSize pubKeyCard
  loopSizeDoor = findLoopSize pubKeyDoor

solve1 = solve

main :: IO ()
main = execute 25 (parseLines number) [
  solve1 ]

