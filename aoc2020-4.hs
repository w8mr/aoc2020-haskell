import Advent
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Char (isDigit)

replace r b input = map repl input where repl c = if c == r then b else c

parse = init .
        (map (map (splitOn ":"))) .
        (map (splitOn " ")) .
        (map (replace '\n' ' ')) .
        splitOn "\n\n"

checkCountryId (key:value:[]) = key /= "cid"

isValid checks xs = ((==7) . length ) $
           foldr filter xs checks

solve checks = length . filter (isValid checks) . parse

solve1 = solve [ checkCountryId ]


between l h i = l <= i && i <= h

parsedNumberBetween l h value = go l h parsedMaybe where
  parsedMaybe = readMaybe value :: Maybe Int
  go _ _ Nothing    = False
  go l h (Just val) = between l h val

checkBirthYear (key:value:[]) = key /= "byr" || parsedNumberBetween 1920 2002 value
checkIssueYear (key:value:[]) = key /= "iyr" || parsedNumberBetween 2010 2020 value
checkExpirationYear (key:value:[]) = key /= "eyr" || parsedNumberBetween 2020 2030 value
checkHairColor (key:(ht:clr):[]) = key /= "hcl" || (ht == '#' && all (\x -> elem x "0123456789abcdef") clr)
checkEyeColor (key:value:[]) = key /= "ecl" || value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
checkPassportId (key:value:[]) = key /= "pid" || (all isDigit value && length value == 9)
checkHeight (key:value:[]) = key /= "hgt" || (unit == "in" && between 59 76 val) || (unit == "cm" && between 150 193 val) where
  parts = splitAt ((length value) - 2) value
  val = fromMaybe 0 $ (readMaybe $ fst parts :: Maybe Int)
  unit = snd parts

solve2 = solve [ checkBirthYear, checkIssueYear, checkExpirationYear,
                 checkHeight, checkHairColor, checkEyeColor,
                 checkPassportId, checkCountryId ]


main :: IO ()
main = execute 4 (id) [
  solve1,
  solve2  ]