module Groups (group, groups) where

import Data.List ((\\))
import qualified Data.Map as Map
import qualified Data.Set as Set


groups :: Ord a => [(a, [a])] -> [[a]]
groups nodes = groups' paths [] where
  paths = Map.fromList nodes

groups' :: Ord a => Map.Map a [a] -> [[a]] -> [[a]]
groups' paths result
  | Map.null paths = result
  | otherwise = groups' paths' result' where
    node = head $ Map.keys paths
    grp = group' paths [node] Set.empty
    paths' = Map.filterWithKey (\k _ -> not $ elem k grp) paths
    result' = result ++ [grp]

group :: Ord a => a -> [(a, [a])] -> [a]
group key nodes = group' paths [key] Set.empty where
   paths = Map.fromList nodes

group' :: Ord a => Map.Map a [a] -> [a] -> Set.Set a -> [a]
group' paths [] result = Set.toList result
group' paths (check:checks) result = group' paths' checks' result' where
  connected = paths Map.! check
  checks' = (checks ++ (Prelude.filter (\x -> not (Set.member x result)) connected)) Data.List.\\ [check]
  paths' = Map.delete check paths
  result' = Set.union result $ Set.fromList ([check] ++ connected)
