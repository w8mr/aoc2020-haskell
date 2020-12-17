{-# Language OverloadedStrings #-}
import Advent
import Data.Maybe(fromMaybe,mapMaybe)
import Data.List(minimumBy, maximumBy,intercalate)
import Data.Ord(comparing)

type Grid2d = [[Char]]
type Grid4d = [(Int,[(Int,[(Int,[Int])])])]
type Coord = (Int,Int,Int,Int)
type Cell = (Coord, Bool)


minimum' [] = 0
minimum' x = minimum x

maximum' [] = 0
maximum' x = maximum x

rangeX :: Grid4d -> [Int]
rangeX grid = [(traverse minimum')..(traverse maximum')] where
  traverse f = (f . map (f . map (f . map (f . snd) . snd) . snd)) grid

rangeY :: Grid4d -> [Int]
rangeY grid = [(traverse minimum')..(traverse maximum')] where
  traverse f = f $ map (  f . map (f . (map fst) . snd) . snd) grid

rangeZ :: Grid4d -> [Int]
rangeZ grid = [(traverse minimum')..(traverse maximum')] where
  traverse f = f $ map (  f . (map fst) . snd) grid

rangeW :: Grid4d -> [Int]
rangeW grid = [(traverse minimum')..(traverse maximum')] where
  traverse f = f $ map fst grid

extend :: [Int] -> [Int]
extend range = [(pred (minimum' range))..(succ (maximum' range))]

run :: [Coord] -> Grid4d -> Grid4d
run dir grid = grid' where
  rules :: Coord -> Bool -> Bool
  rules point active = apply active (countAdjecent dir grid point) where
    apply False c | c == 3 = True
                  | otherwise = False
    apply True  c | c >= 2 && c <= 3 = True
                  | otherwise = False
  grid' = [runCube (w, fromMaybe [] (lookup w grid)) layersRange rowsRange cellsRange | w <- cubesRange] where
    cubesRange = extend (rangeW grid)
    layersRange = extend (rangeZ grid)
    rowsRange = extend (rangeY grid)
    cellsRange = extend (rangeX grid)
    runCube ::  (Int, [(Int, [(Int, [Int])])]) -> [Int] -> [Int] -> [Int] -> (Int, [(Int, [(Int, [Int])])])
    runCube (w,layers) layersRange rowsRange cellsRange= (w, [runLayer w (z, fromMaybe [] (lookup z layers)) rowsRange cellsRange | z <- layersRange])
    runLayer ::  Int -> (Int, [(Int, [Int])]) -> [Int] -> [Int] -> (Int, [(Int, [Int])])
    runLayer w (z,rows) rowsRange cellsRange= (z, [runRow w z (y, fromMaybe [] (lookup y rows)) cellsRange | y <- rowsRange])
    runRow ::  Int -> Int -> (Int, [Int]) -> [Int] -> (Int, [Int])
    runRow w z (y,activeCells) cellsRange = (y, [x | x <- cellsRange, rules (x,y,z,w) (elem x activeCells)] )

displayGrid3 :: Grid4d -> IO ()
displayGrid3 grid = do
  let cubesRange = rangeW grid
  let layersRange = rangeZ grid
  let rowsRange = rangeY grid
  let cellsRange = rangeX grid
  putStrLn $ intercalate "\n\n" ["z="++show z++", w="++show w++"\n"++(layerStr (fromMaybe [] (lookup z (fromMaybe [] (lookup w grid)))) rowsRange cellsRange ) | z <- layersRange, w <- cubesRange]

layerStr :: [(Int, [Int])] -> [Int] -> [Int] -> String
layerStr layer rowsRange cellsRange = intercalate "\n" [rowStr (fromMaybe [] (lookup y layer)) cellsRange | y <- rowsRange]

rowStr :: [Int] -> [Int] -> String
rowStr row cellsRange = [if x `elem` row then '#' else '.' | x <- cellsRange]

index :: Grid2d -> Grid4d
index grid = [(0,[(0,cells)])] where
  cells = [(y-1,[x-1 | x <- [0..(pred (length row))], (row !! x) == '#']) | y <- [0..(pred (length grid))], let row = grid !! y]

solve :: [Coord] -> Grid2d -> Int
solve dir grid2d = go 6 (index grid2d) where
  go 0 grid = countActive3 grid where
  go n grid = go (n-1) (run dir grid)

solve1 = solve dir3
solve2 = solve dir4


countActive3 :: Grid4d -> Int
countActive3 grid = sum (map (\(w,cube) -> sum (map (\(z,layer) -> sum (map (\(y, row) -> length row) layer)) cube)) grid)

dir3 = [ (x,y,z,w) | x <- [-1..1], y <- [-1..1], z <- [-1..1], w <- [0], x/=0 || y/=0 || z/=0 || w/=0]
dir4 = [ (x,y,z,w) | x <- [-1..1], y <- [-1..1], z <- [-1..1], w <- [-1..1], x/=0 || y/=0 || z/=0 || w/=0]

countAdjecent :: [Coord] -> Grid4d -> Coord -> Int
countAdjecent dir grid point = count (isActive . shift point) dir where
  shift (x,y,z,w) (m1,m2,m3,m4) = (x + m1, y + m2, z + m3,w + m4)
  isActive (x,y,z,w) = x `elem` (fromMaybe [] (lookup y (fromMaybe [] (lookup z (fromMaybe [] (lookup w grid ))))))

main :: IO ()
main = execute 17 (lines) [
  solve1 ,
  solve2  ]