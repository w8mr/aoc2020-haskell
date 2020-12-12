import Advent
import Data.List(elemIndex)
import Data.Maybe(fromJust)

format :: Parser (Char, Integer)
format = (,) <$> letterChar <*> number

moves = [('N', (0, -1)),('E', (1, 0)),('S', (0, 1)),('W', (-1, 0))]
move d (x,y) n = let (e,s) = fromJust $ lookup d moves in (x + n * e, y + n * s)
dirs = "NESW"

solve1 xs = abs x + abs y  where
  (dir, (x, y)) = foldl go ('E',(0,0)) xs
  go :: (Char,(Integer, Integer)) -> (Char, Integer) -> (Char,(Integer, Integer))
  go (d1, l) ('F', n) = (d1, move d1 l n)
  go (d1 ,l) ('R', n) = (turnr (div n 90) d1, l)
  go (d1 ,l) ('L', n) = (turnl (div n 90) d1, l)
  go (d1, l) (d2 , n) = (d1, move d2 l n)
  turnr 1 d = dirs !! mod (1 + (fromJust $ elemIndex d dirs)) 4
  turnr n d = turnr (n-1) (turnr 1 d)
  turnl n d = turnr (4-n) d

solve2 xs = abs x + abs y  where
  (_, (x,y)) = foldl go ((10,-1),(0,0)) xs where
  go ((e,s), (x,y)) ('F', n) = ((e,s),(x + n * e, y + n * s))
  go (w, l)         ('R', n) = (rotateR w (div n 90), l)
  go (w, l)         ('L', n) = (rotateL w (div n 90), l)
  go (w, l)         (d  , n) = (move d w n, l)
  rotateR (e,s) 0 = (e,s)
  rotateR (e,s) t = rotateR (-s, e) (t - 1)
  rotateL d     t = rotateR d (4-t)

main :: IO ()
main = execute 12 (parseLines format) [
  solve1,
  solve2  ]