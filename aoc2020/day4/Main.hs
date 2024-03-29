{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Safe #-}

module Main (main) where

import Control.Monad (guard)
import Data.Char (isSpace)
import Data.List (sort)
import Data.Maybe (fromMaybe)

import Parser


data Field =
  Byr
  | Iyr
  | Eyr
  | Hgt
  | Hcl
  | Ecl
  | Pid
  | Cid
  | Unk
  deriving (Show, Eq, Ord)


type Passport = [Field]


-- | Check if the list of fields is valid. To be valid it needs to have one of
-- each field (except 'Cid' which is optional).
isValid :: [Field] -> Bool
isValid fields =
  sorted == [Byr, Iyr, Eyr, Hgt, Hcl, Ecl, Pid, Cid]
  || sorted == [Byr, Iyr, Eyr, Hgt, Hcl, Ecl, Pid]
  where
    sorted = sort fields


-- | Parse a list of Passports from input string. Each field is validated in the process.
passports :: String -> [Passport]
passports = fromMaybe [] . parse (passport `sepBy1` string "\n\n")
  where
    passport :: Parser [Field]
    passport = choice
      [ -- |  (Birth Year) - four digits; at least 1920 and at most 2002.
        field "byr" Byr $ int >>= \i -> guard (i >= 1920 && i <= 2002)

        -- | (Issue Year) - four digits; at least 2010 and at most 2020.
      , field "iyr" Iyr $ int >>= \i -> guard (i >= 2010 && i <= 2020)

        -- | (Expiration Year) - four digits; at least 2020 and at most 2030.
      , field "eyr" Eyr $ int >>= \i -> guard (i >= 2020 && i <= 2030)

        -- | (Height) - a number followed by either cm or in:
        -- If cm, the number must be at least 150 and at most 193.
        -- If in, the number must be at least 59 and at most 76.
      , field "hgt" Hgt do
          i <- int
          u <- anyString
          guard $
            case u of
              "cm" -> i >= 150 && i <= 193
              "in" -> i >= 59 && i <= 76
              _ -> False

        -- | (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
      , field "hcl" Hcl $ char '#' >> (count 6 . choice $ [digit, asciiLower])

        -- | (Eye Color) - exactly one of: amb, blu, brn, gry, grn, hzl, oth.
      , field "ecl" Ecl $ choice
          [ string "amb"
          , string "blu"
          , string "brn"
          , string "grn"
          , string "gry"
          , string "hzl"
          , string "oth"
          ]

        -- | (Passport ID) - a nine-digit number, including leading zeroes.
      , field "pid" Pid $ count 9 digit

        -- | (Country ID) - ignored, missing or not.
      , field "cid" Cid anyString
      ] `sepBy1` space

    -- | Parse a single field of the form 'key:value' where 'value' will be
    -- checked against 'validator' (another Parser expected to match the
    -- 'value' fully).
    --
    -- In case of failure to validate, the value 'Unk' is returned, otherwise 'ctr'.
    field :: String -> Field -> Parser a -> Parser Field
    field key ctr validator =
      string key >> char ':' >> munch (not . isSpace) >>= \value -> return $
        case parse (validator >> eof) value of
          Nothing -> Unk
          _ -> ctr


solve :: String -> String
solve = show . length. filter isValid . passports


main :: IO ()
main = do
  input <- readFile "./input1.txt"
  putStrLn $ solve input
