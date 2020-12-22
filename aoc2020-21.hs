{-# Language OverloadedStrings #-}
import Advent
import Data.List(union, isInfixOf,(\\), nub, sortBy, intercalate)
import Data.Map (empty, alter, toList)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Map as M

format :: Parser ([String],[String])
format = (,) <$> manyTill (many letterChar <* " ") "(contains " <*> (many letterChar `sepBy` ", " <* ")")

possibleIngredientsForAllergen input = map (\(allergen,lines) -> (allergen, intersect' $ map (\l -> fst $ input !! (l-1)) lines)) $ M.toList allergensByLine where
  indexed = zip [1..] input
  addToMap map keys idx = foldl (\map key -> alter (Just .  (`union` [idx]) . (fromMaybe [])) key map) map keys
  allergensByLine = foldl (\map (idx,(ingredients, allergens)) -> addToMap map allergens idx ) M.empty indexed

solve1 input = show $ count (`elem` ingredientsWithoutAllergen) $ concatMap fst input where
  ingredientsWithoutAllergen = (nub $ concatMap fst input) \\ (concat $ map snd $ possibleIngredientsForAllergen input)

solve2 input = intercalate "," $ map snd sortedAllergenIngredient where
  allergenIngredient = snd $ foldl (\(used, found) (key, value) -> let avail = head $ value \\ used in ((avail:used), ((key, avail):found) )) ([], []) $ sortBy (comparing (length . snd)) $ possibleIngredientsForAllergen input
  sortedAllergenIngredient = sortBy (comparing fst) allergenIngredient

main :: IO ()
main = execute 21 (parseLines format) [
  solve1,
  solve2 ]