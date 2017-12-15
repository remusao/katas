#! /usr/bin/env stack
-- stack --resolver lts-9.14 --install-ghc runghc

import Data.Bits

generate :: Int -> Int -> [Int]
generate factor = iterate (\v -> (v * factor) `mod` 2147483647)

match :: (Int, Int) -> Bool
match (a, b) = (a .&. 65535) == (b .&. 65535)

solve1 :: String -> Int
solve1 input =
  let [startA, startB] = map getStart $ lines input
      genA = generate 16807 startA
      genB = generate 48271 startB
   in length . filter match . take 40000000 . zip genA $ genB

solve2 :: String -> Int
solve2 input =
  let [startA, startB] = map getStart $ lines input
      genA = filter (\v -> v `mod` 4 == 0) $ generate 16807 startA
      genB = filter (\v -> v `mod` 8 == 0) $ generate 48271 startB
   in length . filter match . take 5000000 . zip genA $ genB

getStart :: String -> Int
getStart line = read $ drop 24 line

main :: IO ()
main = interact (show . solve2)
