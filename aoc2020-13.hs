{-# Language OverloadedStrings #-}
import Advent
import Data.List (minimumBy)
import Data.Ord (comparing)

bus :: Parser (Integer)
bus = (number <|> -1 <$ "x" )

format :: Parser (Integer,[Integer])
format = (,) <$> number <* "\n" <*> bus `sepBy` "," <* "\n"

leavesIn est id = id - mod est id

solve1 (est, xs) = uncurry (*) $ minimumBy (comparing fst) $ map (\id -> (leavesIn est id, id)) $ filter (>0) xs

solve2 (est, xs) = fst $ crt $ map (\(id,idx) -> (mod (id - idx) id, id)) $ filter ((>0) . fst) $ zip xs [0..]

-- https://stackoverflow.com/questions/35529211/chinese-remainder-theorem-haskell
crt :: (Integral a, Foldable t) => t (a, a) -> (a, a)
crt = foldr go (0,1)
    where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
        where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1

    -- Modular Inverse
    a `inv` m = let (_, i, _) = gcd a m in i `mod` m

    -- Extended Euclidean Algorithm
    gcd 0 b = (b, 0, 1)
    gcd a b = (g, t - (b `div` a) * s, s)
        where (g, s, t) = gcd (b `mod` a) a

main :: IO ()
main = execute 13 (parseInput format) [
  solve1 ,
  solve2  ]