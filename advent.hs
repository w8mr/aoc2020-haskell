module Advent (execute,
               executeString,
               readIntLines,
               readIntWords,
               readIntCells,
               readLines,
               readWords,
               readCells,
               pairsHalf,
               triplesHalf,
               Parser,
               anySingle,
               satisfy,
               char,
               letterChar,
               decimal,
               many,
               number,
               eof,
               sepBy,
               manyTill,
               parseLines,
               parseInput) where

import Data.List (tails)
import Text.Megaparsec (Parsec, anySingle, satisfy, parseMaybe, eof, sepBy, manyTill)
import Text.Megaparsec.Char (char, letterChar)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Control.Applicative (many)
import Data.Void
import Data.Maybe (fromJust)


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


-- | Based on Advent util by glguy
type Parser = Parsec Void String

parseLines :: Parser a -> String -> [a]
parseLines format = map (fromJust . parseMaybe format) . lines

parseInput :: Parser a -> String -> a
parseInput format = fromJust . parseMaybe format


-- | Parse a signed integral number
number :: Integral a => Parser a
number = signed (return ()) decimal