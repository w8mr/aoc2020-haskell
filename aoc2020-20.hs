{-# Language OverloadedStrings #-}
import Advent
import Data.List (reverse,intercalate,nub, sortBy, transpose,find)
import Data.Ord (comparing)
import Data.Maybe(fromJust,isJust)

data Tile = Tile { tnumber :: Integer, array :: [[Bool]] } deriving (Eq, Show)
data PTile = PTile { pnumber :: Integer, edges :: [Int] } deriving (Eq)

instance Show PTile where
  show (PTile pnumber _) = show pnumber

data Orientation = Normal | Rotate90 | Rotate180 | Rotate270 | Flip | FlipRotate90 | FlipRotate180 | FlipRotate270  deriving (Enum, Eq, Show)
orientations = [toEnum 0::Orientation ..]

reorientate = [id, t.f, f.v, f.t, f, t, v, f.t.f] where
  t = transpose
  f = map reverse -- flip left/rigth
  v = reverse     -- flip top/bottom

fRow :: Parser [Bool]
fRow = manyTill ((False <$ ".") <|> (True <$ "#")) "\n"

fTile :: Parser Tile
fTile = Tile <$ "Tile " <*> decimal <* ":\n" <*> manyTill fRow "\n"

format :: Parser [Tile]
format = many fTile

toInt :: [Bool] -> Int
toInt xs = foldl go 0 xs  where
  go a True  = a * 2 + 1
  go a False = a * 2 + 0

toInt' :: [Bool] -> Int
toInt' = toInt . reverse

convertTile :: Tile -> PTile
convertTile tile = PTile (tnumber tile) edges where
  image = array tile
  lr = foldr (\row (l,r) -> ((head row:l),(last row:r))) ([],[]) image
  edges = [ toInt $ snd lr        -- Right                 Normal
          , toInt' $ last image   -- Bottom inverted       Rotate Anticlock wise 90
          , toInt' $ fst lr       -- Left inverted         Rotate Anticlock wise 180
          , toInt $ head image    -- Top                   Rotate Anticlock wise 270
          , toInt $ fst lr        -- Left                  Flip Left/Right
          , toInt $ last image    -- Bottom                Flip Left/Right, Rotate Anticlock wise 90
          , toInt' $ snd lr       -- Right inverted        Flip Left/Right, Rotate Anticlock wise 180
          , toInt' $ head image   -- Top inverted          Flip Left/Right, Rotate Anticlock wise 270
          ]

rightEdge :: (Orientation, PTile) -> Int
rightEdge (orient,tile) = edges tile !! ([0,1,2,3,4,5,6,7] !! (fromEnum orient))

bottomEdge :: (Orientation, PTile) -> Int
bottomEdge (orient,tile) = edges tile !! ([1,2,3,0,5,6,7,4] !! (fromEnum orient))

flipOrientation :: (Orientation, PTile) -> (Orientation, PTile)
flipOrientation (orient,tile) = (toEnum ([4,7,6,5,0,3,2,1] !! fromEnum orient), tile)

rotateOrientation :: (Orientation, PTile) -> (Orientation, PTile)
rotateOrientation (orient,tile) = (toEnum ([7,6,5,4,3,2,1,0] !! fromEnum orient), tile)

findByEdge :: [PTile] -> PTile -> Int -> Maybe (Orientation, PTile)
findByEdge tiles tile edge = ortiles where
  ortiles = fmap (\(tile, (orientation, edge)) -> (orientation, tile)) $
            find (\(tile', (orientation, edge')) -> edge == edge' && tile /= tile') $
            concatMap (\tile -> map (\oredge -> (tile, oredge)) (zip orientations (edges tile))) tiles

matches tiles tile = filter isJust $ map (\edge -> findByEdge tiles tile edge) (take 4 $ edges tile)

toText (tile, matches) = "Tile " ++ show (pnumber tile) ++ " match: " ++ (intercalate ", " (map (show . pnumber) matches))

solve1 tiles = fromIntegral $ product $ map (pnumber . fst) $ take 4 $ sortBy (comparing (length . snd)) $ map (\tile -> (tile, (matches ptiles tile))) ptiles where
  ptiles = map convertTile tiles

solve2 tiles = result   where
  ptiles = map convertTile tiles
  matchesByTile = map (\tile -> (tile, matches ptiles tile)) ptiles
  sorted = sortBy (comparing (length . snd)) $ matchesByTile
  (orient, (tile, _)) = (Normal, head sorted) -- TODO: Find orientation with right and bottom match
  ocorner = (orient, tile)

  nextRight (orient, tile) = nextTileFlipped where
    rEdge = rightEdge (orient, tile)
    nextTile = findByEdge ptiles tile rEdge
    nextTileFlipped = fmap flipOrientation nextTile

  nextBottom (orient, tile) = nextTileFlipped where
    bEdge = bottomEdge (orient, tile)
    nextTile = findByEdge ptiles tile bEdge
    nextTileFlipped = fmap rotateOrientation nextTile

  buildRow otile = go (nextRight otile) [otile] where
    go (Just ot) acc = go (nextRight ot) (ot:acc)
    go Nothing acc = reverse acc

  buildPuzzle otile = go (nextBottom otile) [buildRow otile] where
    go (Just ot) acc = go (nextBottom ot) ((buildRow ot):acc)
    go Nothing acc = reverse acc

  puzzle = buildPuzzle ocorner

  shrink image = map (init . tail) $ init $ tail image

  convert tiles (orient, PTile n _) = tile' where
    (Tile tnummer image) = fromJust $ find(\(Tile n' _) -> n' == n) tiles
    tile' = shrink ((reorientate !! (fromEnum orient)) image)

  grid = map (map (convert tiles)) puzzle
  combined = concat $ map (map concat . transpose) grid

  snakes = concatMap (\orient -> map ((,) orient) $ findSnakes $ (reorientate !! (fromEnum orient)) combined ) orientations

  result = (count (id) (concat combined)) - 15 * (length snakes)

findSnakes image = go (image !! 0) (image !! 1) (image !! 2) (tail image) [] 0 0 where
  go xs@(_   :_   :_   :_   :_   :_   :_   :_   :_   :_   :_   :_   :_   :_   :_   :_   :_   :_   :True:_   :_)
     ys@(True:_   :_   :_   :_   :True:True:_   :_   :_   :_   :True:True:_   :_   :_   :_   :True:True:True:_)
     zs@(_   :True:_   :_   :True:_   :_   :True:_   :_   :True:_   :_   :True:_   :_   :True:_   :_   :_   :_)
     w acc r c = go (tail xs) (tail ys) (tail zs) w ((r,c):acc) r (c+1)
  go (_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:[]) _ _ (x:y:z:xs) acc r c = go x y z (y:z:xs) acc (r+1) 0
  go (x:xs) (y:ys) (z:zs) w acc r c = go xs ys zs w acc r (c+1)
  go _ _ _ (_:_:[]) acc r c = acc


main :: IO ()
main = execute 20 (parseInput format) [
  solve1 ,
  solve2  ]
