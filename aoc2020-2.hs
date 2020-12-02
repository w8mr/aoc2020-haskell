import Advent
import Data.List.Split (splitOn)

splitInput line = (low, high, letter, password) where
  parts = splitOn " " line
  parts2 = splitOn "-" (head parts)
  low = read (head parts2) :: Int
  high = read (parts2 !! 1) :: Int
  letter = head (parts !! 1)
  password = parts !! 2

isValid (low, high, letter, password) = low <= count && count <= high where
  count = length (filter (==letter) password)

isValid2 (low, high, letter, password) = (match1 || match2) && (not (match1 && match2)) where
  match1 = password !! (low - 1) == letter
  match2 = password !! (high - 1) == letter

solve f = length . (filter f) . (map splitInput)

solve1 = solve isValid
solve2 = solve isValid2

main :: IO ()
main = execute 2 (lines) [
  solve1 ,
  solve2  ]