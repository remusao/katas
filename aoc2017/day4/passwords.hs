#! /usr/bin/env stack
-- stack --resolver lts-9.14 --install-ghc runghc

import Data.List (sort)


type Password = String

validate2 :: Password -> Bool
validate2 p = go . sort $ map sort $ words p
  where
    go [] = True
    go [_] = True
    go (w1:w2:xs)
      | w1 == w2 = False
      | otherwise = go (w2:xs)

validate :: Password -> Bool
validate p = go . sort $ words p
  where
    go [] = True
    go [_] = True
    go (w1:w2:xs)
      | w1 == w2 = False
      | otherwise = go (w2:xs)

solve1, solve2 :: [Password] -> Int
solve1 xs = length . filter id $ map validate xs
solve2 xs = length . filter id $ map validate2 xs

main :: IO ()
main = interact (show . solve2 . lines)
