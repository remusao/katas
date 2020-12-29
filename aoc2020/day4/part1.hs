module Main (main) where

import qualified Control.Monad as M
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import qualified Data.Char as C
import Control.Applicative
import Data.List (foldl', sort)

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


passports :: String -> String
passports = show. length. filter isValid .fromMaybe [] . parse (sepBy1 passport (string "\n\n"))
  where
    isValid :: [Field] -> Bool
    isValid fields = sorted == [Byr, Iyr, Eyr, Hgt, Hcl, Ecl, Pid, Cid]  || sorted == [Byr, Iyr, Eyr, Hgt, Hcl, Ecl, Pid]
      where
        sorted = sort fields

    passport :: Parser [Field]
    passport = sepBy1 (choice
      [ byr
      , iyr
      , eyr
      , hgt
      , hcl
      , ecl
      , pid
      , cid
      ]) space

    field :: String -> Parser (String, String)
    field f = do
      k <- string f
      M.void $ char ':'
      v <- munch (not . C.isSpace)
      return (k, v)

    byr :: Parser Field
    byr = do
      (_, value) <- field "byr"
      return (Byr)

    iyr :: Parser Field
    iyr = do
      (_, value) <- field "iyr"
      return (Iyr)

    eyr :: Parser Field
    eyr = do
      (_, value) <- field "eyr"
      return (Eyr)

    hgt :: Parser Field
    hgt = do
      (_, value) <- field "hgt"
      return (Hgt)

    hcl :: Parser Field
    hcl = do
      (_, value) <- field "hcl"
      return (Hcl)

    ecl :: Parser Field
    ecl = do
      (_, value) <- field "ecl"
      return (Ecl)

    pid :: Parser Field
    pid = do
      (_, value) <- field "pid"
      return (Pid)

    cid :: Parser Field
    cid = do
      (_, value) <- field "cid"
      return (Cid)


solve :: String -> String
solve = passports


main :: IO ()
main = do
  input <- readFile "./input.txt"
  -- 254?
  putStrLn $ solve input
