module Advent (execute,
               executeString,
               pairsHalf,
               triplesHalf,
               count,
               Parser,
               anySingle,
               satisfy,
               char,
               letterChar,
               decimal,
               many,
               some,
               optional,
               number,
               (<|>),
               try,
               parseTest,
               eof,
               sepBy,
               sepBy1,
               endBy,
               manyTill,
               parseLines,
               parseInput) where

import Data.List (tails,intersect,union)
import Data.Void (Void)
import Data.Maybe (fromJust)
import Control.Applicative (many, some, optional, (<|>))
import Text.Megaparsec (Parsec, parseTest, anySingle, satisfy, parseMaybe, try, eof, sepBy, sepBy1, endBy, manyTill)
import Text.Megaparsec.Char (char, letterChar)
import Text.Megaparsec.Char.Lexer (decimal, signed)


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

pairsHalf :: [a] -> [(a, a)]
pairsHalf xs = [(x,y) | (x:ys) <- tails xs, y <- ys]

triplesHalf :: [a] -> [(a, a, a)]
triplesHalf xs = [(x,y,z) | (x:ys) <- tails xs, (y:zs) <- tails ys, z <- zs]

count :: (a -> Bool) -> [a] -> Int
count f = length . (filter f)

union' :: Eq a => [[a]] -> [a]  --- Move to Advent
union' = foldr union []

intersect' :: Eq a => [[a]] -> [a]  --- Move to Advent
intersect' = foldr1 intersect


-- | Based on Advent util by glguy
type Parser = Parsec Void String

parseLines :: Parser a -> String -> [a]
parseLines format = map (fromJust . parseMaybe format) . lines

parseInput :: Parser a -> String -> a
parseInput format = fromJust . parseMaybe format

-- | Parse a signed integral number
number :: Integral a => Parser a
number = signed (return ()) decimal