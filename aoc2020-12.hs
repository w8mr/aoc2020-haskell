{-# Language OverloadedStrings #-}
import Advent
import Data.List(elemIndex)
import Data.Maybe(fromJust)

format :: Parser (Char, Integer)
format = (,) <$> letterChar <*> number

solve1 xs = abs x + abs y  where
  (dir, x, y) = foldl go ('E',0,0) xs
  go ('E',x,y) ('F',   n) = ('E', x + n, y    )
  go ('S',x,y) ('F',   n) = ('S', x    , y + n)
  go ('W',x,y) ('F',   n) = ('W', x - n, y    )
  go ('N',x,y) ('F',   n) = ('N', x    , y - n)

  go (d  ,x,y) ('E',   n) = (d  , x + n, y    )
  go (d  ,x,y) ('S',   n) = (d  , x    , y + n)
  go (d  ,x,y) ('W',   n) = (d  , x - n, y    )
  go (d  ,x,y) ('N',   n) = (d  , x    , y - n)

  go (d  ,x,y) ('R',  90) = (turn 1 d,x,y)
  go (d  ,x,y) ('R', 180) = (turn 2 d,x,y)
  go (d  ,x,y) ('R', 270) = (turn 3 d,x,y)
  go (d  ,x,y) ('L',  90) = (turn (-1) d,x,y)
  go (d  ,x,y) ('L', 180) = (turn (-2) d,x,y)
  go (d  ,x,y) ('L', 270) = (turn (-3) d,x,y)

turn t d = d' where
  dirs = ("NESW")
  di = fromJust $ elemIndex d dirs
  d' = dirs !! ((4 + di + t) `mod` 4)

solve2 xs = abs x + abs y  where
  (_, (x,y)) = foldl go ((10,-1),(0,0)) xs where
  go ((e,s), (x,y)) ('F',n) = ((e,s),(x + n * e, y + n * s))
  go (w, l) ('R', 90) = (turn w 1, l)
  go (w, l) ('R', 180) = (turn w 2, l)
  go (w, l) ('R', 270) = (turn w 3, l)
  go (w, l) ('L', 90) = (turn w 3, l)
  go (w, l) ('L', 180) = (turn w 2, l)
  go (w, l) ('L', 270) = (turn w 1, l)
  go (w, l) ('N', n) = (move w ('N',n), l)
  go (w, l) ('E', n) = (move w ('E',n), l)
  go (w, l) ('S', n) = (move w ('S',n), l)
  go (w, l) ('W', n) = (move w ('W',n), l)
  turn (e,s) 0 = (e,s)
  turn (e,s) t = turn (-s, e) (t - 1)
  move (x,y) ('N',n) = (x, y - n)
  move (x,y) ('E',n) = (x + n, y)
  move (x,y) ('S',n) = (x, y + n)
  move (x,y) ('W',n) = (x - n, y)

main :: IO ()
main = execute 12 (parseLines format) [
  solve1 ,
  solve2  ]