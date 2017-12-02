#! /usr/bin/env stack
-- stack --resolver lts-9.14 --install-ghc runghc


import Data.List

checksum :: ([Int] -> Int) -> [[Int]] -> Int
checksum select = foldl' (\c line -> c + select line) 0

solve1, solve2 :: [[Int]] -> Int
solve1 = checksum (\line -> maximum line - minimum line)
solve2 = checksum (\line -> head [
  x `div` y |
    x <- line,
    y <- line,
    x > y,
    x `mod` y == 0
  ])

main :: IO ()
main = interact (show . solve2 . map (map read . words) . lines)
