{-# Language OverloadedStrings #-}
import Advent
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Char (isDigit, isSpace)

field :: Parser (String, String)
field = (,) <$> many letterChar <* ":" <*> manyTill anySingle (satisfy isSpace)

format :: Parser [[(String, String)]]
format = many field `sepBy` "\n"

between value = check (readMaybe value :: Maybe Int) where
  check Nothing _ _    = False
  check (Just val) l h = l <= val && val <= h

checkCountryId (key,value) = key /= "cid"
checkBirthYear (key,value) = key /= "byr" || between value 1920 2002
checkIssueYear (key,value) = key /= "iyr" || between value 2010 2020
checkExpirationYear (key,value) = key /= "eyr" || between value 2020 2030
checkHairColor (key,(ht:clr)) = key /= "hcl" || (ht == '#' && all (\x -> elem x ("0123456789abcdef"::String)) clr)
checkEyeColor (key,value) = key /= "ecl" || value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
checkPassportId (key,value) = key /= "pid" || (all isDigit value && length value == 9)
checkHeight (key,value) = key /= "hgt" || (unit == "in" && between val 59 76) || (unit == "cm" && between val 150 193) where
  (val, unit) = splitAt ((length value) - 2) value

solve checks = count (isValid checks) where
  isValid checks xs = ((==7) . length ) $
           foldr filter xs checks

solve1 = solve [ checkCountryId ]
solve2 = solve [ checkBirthYear, checkIssueYear, checkExpirationYear,
                 checkHeight, checkHairColor, checkEyeColor,
                 checkPassportId, checkCountryId ]

main :: IO ()
main = execute 4 (parseInput format) [
  solve1,
  solve2  ]