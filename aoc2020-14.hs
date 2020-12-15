{-# Language OverloadedStrings #-}
import Advent
import Data.Bits (clearBit, setBit, testBit, (.&.), (.|.))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

data Instruction = Mask [Maybe Bool] | Mem Int Int deriving (Show, Eq)
data State = State { mask :: [Maybe Bool], mem :: IntMap Int} deriving (Show, Eq)

fBit :: Parser (Maybe Bool)
fBit = (Just False <$ "0") <|> (Just True <$ "1") <|> (Nothing <$ "X")

fMask :: Parser (Instruction)
fMask = Mask <$ "mask = " <*> many fBit

fMem :: Parser (Instruction)
fMem = Mem <$ "mem[" <*> decimal <* "] = " <*> decimal

format :: Parser Instruction
format = fMask <|> fMem

applyMask :: [Maybe Bool] -> Int -> Int
applyMask mask val = (val .|. m1) .&. m2 where
   (m1, m2) = foldl (\(a1,a2) c -> (a1*2 + p c, a2*2 + q c)) (0,0) mask
   p (Just True) = 1
   p _ = 0
   q (Just False) = 0
   q _ = 1

updateMem :: IntMap Int -> Int -> Int -> IntMap Int
updateMem mem addr val = IntMap.insert addr val mem

process :: State -> Instruction -> State
process (State _    mem) (Mask mask) = State mask mem
process (State mask mem) (Mem addr val) = State mask (updateMem mem addr (applyMask mask val))

toMask :: Int -> [Maybe Bool]
toMask val = [ Just (testBit val i) | i <- [35, 34..0] ]

toAddress :: [Maybe Bool] -> Int
toAddress mask = foldl go 0 (zip [35, 34..0] mask) where
  go val (i, Just True) = setBit val i
  go val _              = val

floatingAddress :: [Maybe Bool] -> Int -> [Maybe Bool]
floatingAddress mask addr = map go (zip mask (toMask addr)) where
  go :: (Maybe Bool, Maybe Bool) -> Maybe Bool
  go (Just False, y) = y
  go (x, _) = x

floatingToReal :: [Maybe Bool] -> [Int]
floatingToReal = go [[]] where
  go :: [[Maybe Bool]] -> [Maybe Bool] -> [Int]
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
initState = State [Nothing | _ <- [0..35]] IntMap.empty

solve :: (State -> Instruction -> State) -> [Instruction] -> Int
solve f instructions = sum mem where
  State mask mem = go initState instructions
  go :: State -> [Instruction] -> State
  go state = foldl f state

solve1 = solve process
solve2 = solve process2

main :: IO ()
main = execute 14 (parseLines format) [
  solve1 ,
  solve2  ]



