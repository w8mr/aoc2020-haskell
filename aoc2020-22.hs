{-# Language OverloadedStrings #-}
import Advent
import Data.List (find,union)
import Data.Maybe (fromJust,isJust)

fPlayer :: Parser [Int]
fPlayer = (decimal <* "\n") `manyTill` "\n"
format :: Parser([Int],[Int])
format = (,) <$ "Player 1:\n" <*> fPlayer <* "Player 2:\n"<*> fPlayer

playRound1 = playRound False
playRound2 = playRound True
playRound part2 (x:xs,y:ys) | isSubgame = playSubgame
                            | x > y = winX
                            | y > x = winY
  where
    isSubgame = part2 && x <= length xs && y <= length ys
    playSubgame = winner $ playGame playRound2 (take x xs,take y ys)
    winner (_,[]) = winX
    winner ([],_) = winY
    winX = (xs ++ [x, y], ys)
    winY = (xs, ys ++ [y, x])

playGame f input = (snd . fromJust . find (\(_,(p1,p2)) -> null p1 || null p2) . (iterate (detect f))) ([],input) where
  detect f (played, decks@(xs, ys)) | decks `elem` played = (played, (xs++ys, []))
                                    | otherwise = (decks:played, f decks)

solve f = fst . score . playGame f where
  score = (foldr (\nr (sum,idx) -> (sum + nr * idx, succ idx)) (0,1)) . uncurry (union)

solve1 = solve playRound1
solve2 = solve playRound2

main :: IO ()
main = execute 22 (parseInput format) [
  solve1 ,
  solve2  ]
