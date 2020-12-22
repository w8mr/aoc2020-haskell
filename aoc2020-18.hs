{-# Language OverloadedStrings #-}
import Advent
import Data.Char (digitToInt, isDigit)
import Control.Monad (forM)

data Op = Product | Sum deriving (Eq, Show)
data Calc = Empty | Paren Calc | Number Int | Operator Op Calc Calc  deriving (Eq, Show)
--fCalc = (Number <$> number)
--fParen = do
--           char '('
--           calc <- fCalc
--           char ')'
--           return calc
--fSum = try (Sum <$> fCalc <* " + " <*> fCalc)
--fProduct = try (Product <$> fCalc <* " * " <*> fCalc)

parser1 :: String -> Calc
parser1 s = head $ go s [Empty] where
  go [] st = st
  go (x:xs) st | x == ' ' = go xs st
  go (x:xs) (st) | x == '(' = go xs (Empty:st)
  go (x:xs) (c:(Operator o l _):st) | x == ')' = go xs ((Operator o l c):st)
  go (x:xs) (c:(Number n):st) | x == ')' = go xs (c:st)
  go (x:xs) (c:(Empty):st) | x == ')' = go xs (c:st)
  go (x:xs) (c:st) | otherwise = go xs ((go2 x c):st)
  go2 x st | x == '+' = (Operator Sum st (Number 0))
  go2 x st | x == '*' = (Operator Product st (Number 0))
  go2 x (Empty) | isDigit x = Number (digitToInt x)
  go2 x (Number n) | isDigit x = (Number (n * 10 + (digitToInt x)))
  go2 x (Operator o l (Number n)) | isDigit x = (Operator o l ((Number (n * 10 + (digitToInt x)))))
  go2 x _ = error ("x:"++(show x))

parser2 :: String -> Calc
parser2 s = snd $ go (s,Empty) where
  go ([]      , st)                                    = ([], st)
  go ((' ':xs), st)                                    = go (xs, st)
  go ((x  :xs), Empty)                     | isDigit x = go (xs, Number (digitToInt x))
  go ((x  :xs), Number n)                  | isDigit x = go (xs, Number (n * 10 + (digitToInt x)))
  go ((x  :xs), Operator o st Empty)       | isDigit x = go (xs, Operator o st (Number (digitToInt x)))
  go ((x  :xs), Operator o st (Number n))  | isDigit x = go (xs, Operator o st (Number (n * 10 + (digitToInt x))))

  go ((x  :xs), Empty)                     | x == '('  = go (go (xs, Empty))
  go ((x  :xs), Operator o st Empty)       | x == '('  = go (xs', (Operator o st st'))  where (xs', st') = go (xs, Empty)
  go ((x  :xs), st)                        | x == ')'  = (xs, st)

  go ((x  :xs), st)                        | x == '+'  = go (xs, Operator Sum st Empty)
  go ((x  :xs), st)                        | x == '*'  = (xs', (Operator Product st st')) where (xs', st') = go (xs, Empty)
  go ((x  :_ ), st)                                    = error ("Charater not allowed: "++(show x)++" Stack: "++ (show st))
--           ("(2 * (3 * 4) + 5) + 6",40),
test :: IO [()]
test = do
  let tests = [("1", 1),
           ("12", 12),
           ("1+1", 2),
           ("2*2", 4),
           ("1+2*3+4", 21),
           ("1+2*3+4*5+6", 231),
           ("1*2+3*4", 20),
           ("(1)",1),
           ("1+(2*3)+4",11),
           ("1 + 1 * 1 + 1 + 1",6),
           ("(1 + 1 * 1 + 1) + 1",5),
           ("((1) * (1 + 2 * 8 + 4) + 5)",41),
           ("((1) * (1 + 2 * 8 + 4))",36),
           ("1 + (2 * 3) + (4 * (5 + 6))",51),
           ("2 * 3 + (4 * 5)",46),
           ("5 + (8 * 3 + 9 + 3 * 4 * 3)",1445),
           ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",669060),
           ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2",23340),
           ("((6 * 9) * (15 * 14) + 6) + 2 + 4 * 2",23340),
           ("((54) * (15 * 14) + 6) + 2 + 4 * 2",23340),
           ("(54 * (15 * 14) + 6) + 2 + 4 * 2",23340),
           ("(54 * (15 * 14) + 6) + 6 * 2",23340),
           ("(54 * (15 * 14) + 6) + 6",11670),
           ("(2 * (3 * 4) + 5) + 6",40),
           ("", 0)] :: [(String, Int)]
  let results = map (\(test, expected) -> let parsed = parser2 test; actual = calc parsed in (expected, actual, test, parsed)) tests
  forM (filter (\(e,a,_,_) -> e /= a) results) (\(expected, actual, test, parsed) -> do
          putStrLn $ "Test failed " ++ test ++ " expectd " ++ show expected ++ " actual " ++ show actual ++ " parsed " ++ show parsed
          )

calc :: Calc -> Int
calc (Empty) = 0
calc (Number n) = n
calc (Operator Product r l) = (calc r) * (calc l)
calc (Operator Sum r l) = (calc r) + (calc l)

solve = sum . map (calc . parser2)

solve1 = sum . map (calc . parser1)
solve2 = sum . map (calc . parser2)

main :: IO ()
main = execute 18 (lines) [
  solve1 ,
  solve2  ]