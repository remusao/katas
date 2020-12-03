{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.Maybe (mapMaybe)

import Parser (Parser(..), parse, char, int, anyChar, anyString, string)


data Password = Password
  { low :: Int
  , up :: Int
  , chr :: Char
  , password :: String
  }


parseInput :: String -> [Password]
parseInput = mapMaybe (parse password) . lines
    where
        password :: Parser Password
        password =
            Password <$>
              int
              <*> (char '-' *> int)
              <*> (char ' ' *> anyChar)
              <*> (string ": " *> anyString)


solve1 :: [Password] -> Int
solve1 = length . filter isValid
  where
    isValid :: Password -> Bool
    isValid Password { low, up, chr, password } =
      let count = length . filter (== chr) $ password
       in count >= low && count <= up


solve2 :: [Password] -> Int
solve2 = length . filter isValid
  where
    isValid :: Password -> Bool
    isValid Password { low, up, chr, password } = go password 1 False
      where
        go :: String -> Int -> Bool -> Bool
        go [] _ acc = acc
        go (c:cs) idx acc =
          go cs (idx + 1) (
            if idx == low || idx == up
               then acc /= (c == chr)
               else acc
          )


main :: IO ()
main = interact $ show . solve2 . parseInput
