{-# Language OverloadedStrings #-}
import Advent
import Data.Maybe(isJust)

type Grid = [[Char]]
type Coord = (Int,Int)
type Seat = (Coord, Bool)
type IGrid = ((Int, Int), [Seat])

run :: (IGrid -> Coord -> Int) -> Int -> IGrid -> IGrid
run f t grid@(dimension,seats) = (dimension,seats') where
  rules :: Seat -> Seat
  rules seat@(point, occupied) = apply occupied (f grid point) where
    apply False c | c == 0 = flp seat
                  | otherwise = seat
    apply True  c | c >= t = flp seat
                  | otherwise = seat
    flp ((x,y), occupied) = ((x,y), not occupied)
  seats' = map rules seats

index :: Grid -> IGrid
index grid = ((maxWidth, maxHeight), seats) where
  maxHeight = ((length grid) - 1)
  maxWidth = ((length $ head grid)-1)
  seats = [((x,y),c=='#') | y <- [0..maxHeight], x <- [0..maxWidth], c <- [(grid !! y) !! x], c /= '.']

solve :: (IGrid -> Coord -> Int) -> Int -> Grid -> Int
solve f t grid = go (index grid) where
  go grid@(dim, seats) = if (grid'==grid) then count snd seats else go grid' where
    grid' = run f t grid

dir = [(-1,-1),(0,-1),(1,-1),(-1,0),(1,0),(-1,1),(0,1),(1,1)]

countAdjecent :: IGrid -> Coord -> Int
countAdjecent ((w,h), seats) point = count (isOccupied . findSeat . shift point) dir where
  shift (x,y) (r,d) = (x + r, y + d)
  findSeat (x,y) = lookup (x,y) seats
  isOccupied Nothing = False
  isOccupied (Just occupied) = occupied

countAdjecent2 :: IGrid -> Coord -> Int
countAdjecent2 ((w,h), seats) point = count (isOccupied . findSeat) dir where
  findSeat dir = go 1 dir where
    go m (r,d) = if inBound shifted then
                   let seat = lookup shifted seats in
                     if isJust seat then seat else go (m+1) (r,d)
                 else Nothing where
      shifted = (shift point (m * r, m * d))
      inBound (x,y) = 0<=x && x<=w && 0<=y && y<=h
      shift (x,y) (r,d) = (x + r, y + d)
  isOccupied Nothing = False
  isOccupied (Just occupied) = occupied

solve1 = solve countAdjecent 4
solve2 = solve countAdjecent2 5

main :: IO ()
main = execute 11 (lines) [
 -- solve1 ,
  solve2  ]