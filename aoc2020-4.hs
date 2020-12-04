import Advent
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Char (isDigit)

replace r b input = map repl input where repl c = if c == r then b else c

parse = (map (map (splitOn ":"))) . (map (splitOn " ")) . (map (replace '\n' ' ')) . splitOn "\n\n"

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
checkCountryId (key:value:[]) = key /= "cid"
checkHeight (key:value:[]) = key /= "hgt" || (unit == "in" && between 59 76 val) || (unit == "cm" && between 150 193 val) where
  parts = splitAt ((length value) - 2) value
  val = fromMaybe 0 $ (readMaybe $ fst parts :: Maybe Int)
  unit = snd parts
checkEmpty = ((==2) . length)

isValid checks xs = ((==7) . length ) $
           foldr filter xs checks

solve checks = length . filter (isValid checks)

solve1 = solve [ checkCountryId, checkEmpty ]
solve2 = solve [ checkBirthYear, checkIssueYear, checkExpirationYear,
                 checkHeight, checkHairColor, checkEyeColor,
                 checkPassportId, checkCountryId, checkEmpty ]

main :: IO ()
main = execute 4 (parse) [
  solve1 ,
  solve2  ]