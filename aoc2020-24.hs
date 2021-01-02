{-# Language OverloadedStrings #-}
import Advent
import Prelude hiding (flip)
import Data.IntMap (empty, singleton,alter, toList, findWithDefault)
import qualified Data.IntMap as M
import Data.Maybe (fromMaybe)

format :: Parser [HexDir]
format = many ((NW <$ "nw") <|> (NE <$ "ne") <|> (E <$ "e") <|> (SE <$ "se") <|> (SW <$ "sw") <|> (W <$ "w"))

data HexDir = NW | NE | E | SE | SW | W deriving (Eq,Show,Enum)
data Color = White | Black deriving (Eq,Show)

dir NW = (-1, -1)
dir NE = ( 1, -1)
dir E  = ( 2,  0)
dir SE = ( 1,  1)
dir SW = (-1,  1)
dir W  = (-2,  0)

type Grid = M.IntMap (M.IntMap Color)
type Coord = (Int,Int)

move :: HexDir -> Coord -> Coord
move d (x,y) = (x + w, y + s) where
  (w,s) = dir d

flip :: Coord -> Grid -> Grid
flip (x,y) grid = M.alter flipOnRow y grid where
  flipOnRow Nothing = Just (M.singleton x Black)
  flipOnRow (Just row) = Just (M.alter flipTile x row)
  flipTile Nothing = Just Black
  flipTile (Just cell) = Just (flip' cell)
  flip' Black = White
  flip' White = Black

origin :: Coord
origin = (0,0)

solve input = grid where
  grid = foldl procesLine M.empty input
  procesLine grid line = let endTile = foldl procesDirs origin line in flip endTile grid
  procesDirs coord dir = move dir coord


solve1 input = count ((==Black) . snd) $ concatMap (M.toList . snd) (M.toList grid) where
  grid = solve input

solve2 input = count ((==Black) . snd) $ concatMap (M.toList . snd) (M.toList $ (iterate round grid) !! 100) where
  grid = solve input
  round grid = newgrid where
    lowerY = fst $ fromMaybe (0,M.empty) $ M.lookupMin grid
    upperY = fst $ fromMaybe (0,M.empty) $ M.lookupMax grid
    rows = M.elems grid
    lowerX = minimum $ map (fst . fromMaybe (0,White) . M.lookupMin) rows
    upperX = maximum $ map (fst . fromMaybe (0,White) . M.lookupMax) rows
    result = (lowerX, lowerY, upperX, upperY)
    newgrid = foldr (\row g -> M.alter ((processRow row) . fromMaybe M.empty) row g) grid [(lowerY-1)..(upperY+1)]
    processRow row rowMap = Just $ foldr (\col rowMap -> M.alter (toDefault . (processCell row col) . fromMaybe White) col rowMap) rowMap [(lowerX-1)..(upperX+1)] where
      toDefault White = Nothing
      toDefault Black = Just Black
    processCell rIdx cIdx col = go col (countAdjacentBlack rIdx cIdx grid) where
      go Black 0 = White
      go Black c | c > 2 = White
      go White 2 = Black
      go col _ = col
    countAdjacentBlack r c grid = count (==Black) [ M.findWithDefault White c' (M.findWithDefault M.empty r' grid) | d <- [(toEnum 0::HexDir)..], let (c',r') = move d (c,r) ]

main :: IO ()
main = execute 24 (parseLines format) [
  solve1 ,
  solve2  ]