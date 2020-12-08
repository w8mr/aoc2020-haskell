{-# Language OverloadedStrings #-}
import Advent
import Data.List.Split (splitOn)

format :: Parser (Int, Int, Char, String)
format = (,,,) <$> number <* "-" <*> number <* " " <*> anySingle <* ": " <*> many anySingle

isValid (low, high, letter, password) = low <= count && count <= high where
  count = length (filter (==letter) password)

isValid2 (low, high, letter, password) = match low /= match high where
  match n = password !! (n - 1) == letter

solve f = length . (filter f)

solve1 = solve isValid
solve2 = solve isValid2

main :: IO ()
main = execute 2 (parseLines format) [
  solve1 ,
  solve2  ]