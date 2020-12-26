{-# Language OverloadedStrings, BlockArguments #-}
import Advent
import Data.List (elemIndex, intercalate, find)
import Data.Maybe (fromJust)
import Data.Char (intToDigit, digitToInt)
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

input1 = [3,8,9,1,2,5,4,6,7] :: [Int]
input = [5,8,6,4,3,9,1,7,2] :: [Int]
input2 = input ++ [10..60]

move xs = out where
  cycle_current = (take (length xs) . drop 1 . cycle) xs
  (pick, list) = splitAt 3 cycle_current
  current = last list
  destination = let low = filter (<current) list in if null low then maximum list else maximum low
  (before,after) = splitAt ((+1) $ fromJust $ elemIndex destination list) list
  out = concat [before,pick,after]

game n input = numbers where
  result = last $ take (n+1) $ iterate move input
  numbers = take 8 $ drop ((+1) $ fromJust $ elemIndex 1 result) $ cycle result


display input = out where
  current = last input
  cycle1 = take (length input) $ drop ( fromJust $ elemIndex 1 input) $ cycle input
  (before,(c:after)) = splitAt (fromJust $ elemIndex current input) input
  out = intercalate "," $ map (\n -> if n == c then "("++show n++")" else show n) cycle1

simulate _ 0 = do
  return ()
simulate input n = do
  print $ display $ move input
  simulate (move input) (pred n)
  return ()

solve = id

s1 xs = (read $ map intToDigit $ tail $ play 100 xs ) :: Int
s2 xs = n1*n2 where
  input = xs ++ [10..1000000]
  (_:n1:n2:_) = play 10000000 input

solve1 = s1
solve2 = s2

main :: IO ()
main = execute 23 (map (digitToInt) .  head . lines) [
  solve1 ,
  solve2  ]

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


play' :: Int -> [Int] -> UArray Int Int
play' n input = runSTUArray $ do
    let size = length input
    linkedList <- newArray_ (1,size) :: ST s (STUArray s Int Int)


    --build linked list
    zipWithM_ (writeArray linkedList) input (tail (cycle input))

    play'' n linkedList (head input)

play :: Int -> [Int] -> [Int]
play n = unlinkList . play' n

unlinkList :: UArray Int Int -> [Int]
unlinkList linkedList = reverse list where
  ll = assocs linkedList
  size = length ll - 1
  go 0 _ l = l
  go i ll (x:xs) = let linked = fromJust (lookup x ll) in go (i - 1) ll (linked:x:xs)
  list = go size ll [1]

primesUpto :: Int -> [Int]
primesUpto n = [p | (p, True) <- assocs $ sieve n]

sieve :: Int -> UArray Int Bool
sieve n = runSTUArray $ do
    sieve <- newArray (2, n) True
    forM_ [2..n] $ \p -> do
        isPrime <- readArray sieve p
        when isPrime $ do
            forM_ [p*2, p*3 .. n] $ \k -> do
                writeArray sieve k False
    return sieve