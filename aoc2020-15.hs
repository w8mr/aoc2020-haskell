{-# Language OverloadedStrings #-}
import Advent
import Data.IntMap.Strict (IntMap, insert, (!?), fromList)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)


format :: Parser [Int]
format = decimal `sepBy` "," <* "\n"

--vanEyk i = (init i) ++ (map snd (scanl (\(m,x) i -> (insert x i m, i - fromMaybe i (m !? x))) (IntMap.fromList (zip (init i) [1..]), last i) [(length i)..]))

-- Solution get stack exception in interactive mode
--solve n xs = snd $ foldl go (initMap, last xs) [(length xs)..(n-1)] where
--  initMap = IntMap.fromList (zip (init xs) [1..])
--  go (m, x) i = (insert x i m, i - fromMaybe i (m !? x))

solve n xs = go [(length xs)..(n-1)] (last xs) (IntMap.fromList (zip (init xs) [1..])) where
  go [] x _ = x
  go (i:is) x m = p (m !? x) where
    p (Just v) = go is (i-v) (insert x i m)
    p Nothing  = go is 0     (insert x i m)


solve1 = solve 2020
solve2 = solve 30000000

main :: IO ()
main = execute 15 (parseInput format) [
  solve1 ,
  solve2  ]