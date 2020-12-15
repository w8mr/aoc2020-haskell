{-# Language OverloadedStrings #-}
import Advent
import Data.IntMap.Strict (IntMap, insert, (!?))
import qualified Data.IntMap.Strict as IntMap

format :: Parser [Int]
format = decimal `sepBy` "," <* "\n"

solve q xs = go (length xs) (last xs) (IntMap.fromList (zip (init xs) [1..])) where
 -- go :: Int -> Int -> IntMap Int -> Int
  go i    x m | i == q    = x
              | otherwise = p (m !? x) where
    p (Just v)= go (i+1) (i-v) (insert x i m)
    p Nothing = go (i+1) 0     (insert x i m)

solve1 = solve 2020
solve2 = solve 30000000

main :: IO ()
main = execute 15 (parseInput format) [
  solve1 ,
  solve2  ]