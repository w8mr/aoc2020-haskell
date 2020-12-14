{-# Language OverloadedStrings #-}
import Advent
import Data.Bits (clearBit, setBit, testBit)


data Instruction = Mask [Maybe Bool] | Mem Integer Integer deriving (Show, Eq)
data State = State { mask :: [Maybe Bool], mem :: [(Integer,Integer)]} deriving (Show, Eq)

fBit :: Parser (Maybe Bool)
fBit = (Just False <$ "0") <|> (Just True <$ "1") <|> (Nothing <$ "X")

fMask :: Parser (Instruction)
fMask = Mask <$ "mask = " <*> many fBit

fMem :: Parser (Instruction)
fMem = Mem <$ "mem[" <*> number <* "] = " <*> number

format :: Parser Instruction
format = fMask <|> fMem

applyMask :: [Maybe Bool] -> Integer -> Integer
applyMask mask val = foldl update val (zip [35, 34..0] mask) where
  update :: Integer -> (Int, Maybe Bool) -> Integer
  update val (i, Just True)  = setBit val i
  update val (i, Just False) = clearBit val i
  update val _               = val

updateMem :: [(Integer,Integer)] -> Integer -> Integer -> [(Integer,Integer)]
updateMem mem addr val = go mem (lookup addr mem) val where
  go mem (Just _) val = map (\(a, old) -> if a == addr then (a, val) else (a,old) ) mem
  go mem Nothing val = ((addr, val):mem)

process :: State -> Instruction -> State
process (State _    mem) (Mask mask) = State mask mem
process (State mask mem) (Mem addr val) = State mask (updateMem mem addr (applyMask mask val))

toMask :: Integer -> [Maybe Bool]
toMask val = [ Just (testBit val i) | i <- [35, 34..0] ]

toAddress :: [Maybe Bool] -> Integer
toAddress mask = foldl go 0 (zip [35, 34..0] mask) where
  go val (i, Just True) = setBit val i
  go val _              = val

floatingAddress :: [Maybe Bool] -> Integer -> [Maybe Bool]
floatingAddress mask addr = map go (zip mask (toMask addr)) where
  go :: (Maybe Bool, Maybe Bool) -> Maybe Bool
  go (Just False, y) = y
  go (x, _) = x

floatingToReal :: [Maybe Bool] -> [Integer]
floatingToReal = go [[]] where
  go :: [[Maybe Bool]] -> [Maybe Bool] -> [Integer]
  go addresses [] = map toAddress addresses
  go addresses (Just b:xs) = go (map (\x -> x ++ [Just b]) addresses) xs
  go addresses (Nothing:xs) = go (concatMap (\x -> [x ++ [Just False],x ++ [Just True]]) addresses) xs


process2 :: State -> Instruction -> State
process2 (State _    mem) (Mask mask) = State mask mem
process2 (State mask mem) (Mem addr val) = State mask mem' where
  fAddr = floatingAddress mask addr
  rAddr = floatingToReal fAddr
  updates = map (\x -> (x, val)) rAddr
  mem' = foldl (\m (addr, val) -> updateMem m addr val) mem updates

initState :: State
initState = State [Nothing | _ <- [0..35]] []

solve :: (State -> Instruction -> State) -> [Instruction] -> Integer
solve f instructions = sum $ map snd mem where
  State mask mem = go initState instructions
  go :: State -> [Instruction] -> State
  go state = foldl f state

solve1 = solve process
solve2 = solve process2

main :: IO ()
main = execute 14 (parseLines format) [
  solve1 ,
  solve2  ]