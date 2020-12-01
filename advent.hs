module Advent (execute, executeString, readIntLines, readIntWords, readIntCells, readLines, readWords, readCells, pairsHalf, triplesHalf) where

import Data.List (tails)

readDayInput :: Int -> IO (String)
readDayInput day = readFile ("input/day" ++ show day ++ ".txt")

execute' :: Show b => (String -> a) -> [(a -> b)] -> String -> String
execute' pre parts text = output where
  input = pre text
  output = concat $ map (\(index, part) ->
     concat [ "Part ", show (index+1), ": ", show $ part (input), "\n" ]) $ zip [0..] parts

execute :: Show b => Int -> (String -> a) -> [(a -> b)] -> IO ()
execute day preall parts = execute' preall parts <$> readDayInput day >>= putStrLn

executeString :: Show b => String -> (String -> a) -> [(a -> b)] -> String
executeString text preall parts = execute' preall parts text

readIntLines :: String -> [Int]
readIntLines = map read . lines

readIntWords :: String -> [Int]
readIntWords = map read . words

readIntCells :: String -> [[Int]]
readIntCells = map (map read . words) . lines

readLines :: String -> [String]
readLines = lines

readWords :: String -> [String]
readWords = words

readCells :: String -> [[String]]
readCells = map words . lines


pairsHalf :: [a] -> [(a, a)]
pairsHalf xs = [(x,y) | (x:ys) <- tails xs, y <- ys]

triplesHalf :: [a] -> [(a, a, a)]
triplesHalf xs = [(x,y,z) | (x:ys) <- tails xs, (y:zs) <- tails ys, z <- zs]

