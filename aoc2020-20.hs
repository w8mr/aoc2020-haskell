{-# Language OverloadedStrings #-}
import Advent
import Data.List (reverse,intercalate,nub, sortBy)
import Data.Ord (comparing)

data Tile = Tile { tnumber :: Integer, array :: [[Bool]] } deriving (Eq, Show)
data PTile = PTile { pnumber :: Integer, edges :: [Int] } deriving (Eq)

instance Show PTile where
  show (PTile pnumber _) = show pnumber

fRow :: Parser [Bool]
fRow = manyTill ((False <$ ".") <|> (True <$ "#")) "\n"


fTile :: Parser Tile
fTile = Tile <$ "Tile " <*> decimal <* ":\n" <*> manyTill fRow "\n"

format :: Parser [Tile]
format = many fTile

tile1 = Tile {tnumber = 2311, array = [[False,False,True,True,False,True,False,False,True,False],[True,True,False,False,True,False,False,False,False,False],[True,False,False,False,True,True,False,False,True,False],[True,True,True,True,False,True,False,False,False,True],[True,True,False,True,True,False,True,True,True,False],[True,True,False,False,False,True,False,True,True,True],[False,True,False,True,False,True,False,False,True,True],[False,False,True,False,False,False,False,True,False,False],[True,True,True,False,False,False,True,False,True,False],[False,False,True,True,True,False,False,True,True,True]]}

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
  edges = [ toInt $ head image    -- Top
          , toInt $ snd lr        -- Right                 Rotate Anticlock wise 1
          , toInt' $ last image   -- Bottom inverted       Rotate Anticlock wise 2
          , toInt' $ fst lr       -- Left inverted         Rotate Anticlock wise 3
          , toInt' $ head image   -- Top inverted          Flip Left/Right
          , toInt $ fst lr        -- Left                  Flip Left/Right, Rotate Anticlock wise 1
          , toInt $ last image    -- Bottom                Flip Left/Right, Rotate Anticlock wise 2
          , toInt' $ snd lr       -- Right inverted        Flip Left/Right, Rotate Anticlock wise 3
          ]

findByEdge tiles edge = filter ((any (==edge)) . edges) tiles

possibleMatch tiles tile = map (filter (/=tile)) $ map (\edge -> findByEdge tiles edge) (edges tile)

toText (tile, matches) = "Tile " ++ show (pnumber tile) ++ " match: " ++ (intercalate ", " (map (show . pnumber) matches))

solve1 tiles = product $ map (pnumber . fst) $ take 4 $ sortBy (comparing (length . snd)) $ map (\tile -> (tile, (nub $ concat $ (possibleMatch ptiles tile)))) ptiles where
  ptiles = map convertTile tiles

solve2 tiles = corner  where
                 ptiles = map convertTile tiles
                 matches = map (\tile -> (tile, possibleMatch ptiles tile)) ptiles
                 sorted = sortBy (comparing (length . concat . snd)) $ matches
                 corners = take 4 $ sorted
                 corner = (!!1) $ sortBy (comparing (pnumber . fst)) corners


main :: IO ()
main = execute 99 (parseInput format) [
 -- solve1 ,
  solve2  ]