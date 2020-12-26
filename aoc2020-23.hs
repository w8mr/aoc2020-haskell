{-# Language OverloadedStrings, BlockArguments #-}
import Advent
import Data.List (elemIndex, intercalate, find)
import Data.Maybe (fromJust)
import Data.Char (intToDigit, digitToInt)
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

play :: Int -> [Int] -> [Int]
play n = unlinkList . play' n

unlinkList :: UArray Int Int -> [Int]
unlinkList linkedList = reverse list where
  ll = assocs linkedList
  size = length ll - 1
  go 0 _ l = l
  go i ll (x:xs) = let linked = fromJust (lookup x ll) in go (i - 1) ll (linked:x:xs)
  list = go size ll [1]

play' :: Int -> [Int] -> UArray Int Int
play' n input = runSTUArray $ do
    let size = length input
    linkedList <- newArray_ (1,size) :: ST s (STUArray s Int Int)

    --build linked list
    zipWithM_ (writeArray linkedList) input (tail (cycle input))

    play'' n linkedList (head input)

play'' :: Int -> STUArray s Int Int -> Int -> ST s (STUArray s Int Int)
play'' 0 linkedList current = do
  return linkedList
play'' iterations linkedList current =
  do
    n1 <- readArray linkedList current
    n2 <- readArray linkedList n1
    n3 <- readArray linkedList n2
    next <- readArray linkedList n3

    -- find destination
    (lo,hi) <- getBounds linkedList
    let size = hi-lo +1

    let numbers = (n3:n2:n1:[])
    let destination = fromJust $ find (not . (`elem` numbers)) $ iterate (succ . (`mod` size) . pred . pred) ((succ . (`mod` size) . pred . pred) current)

    -- Remove numbers from list
    writeArray linkedList current next

    -- Add numbers at destination
    dest <- readArray linkedList destination
    writeArray linkedList n3 dest
    writeArray linkedList destination n1

    --recurse
    play'' (iterations - 1) linkedList next

solve1 xs = (read $ map intToDigit $ tail $ play 100 xs ) :: Int
solve2 xs = n1*n2 where
  input = xs ++ [10..1000000]
  (_:n1:n2:_) = play 10000000 input

main :: IO ()
main = execute 23 (map (digitToInt) .  head . lines) [
  solve1 ,
  solve2  ]
