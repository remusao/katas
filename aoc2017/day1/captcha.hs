#! /usr/bin/env stack
-- stack --resolver lts-9.14 --install-ghc runghc


import Data.Char

digits :: String -> [Int]
digits = map digitToInt . filter isDigit

captcha :: [Int]-> Int -> Int
captcha xs n = sum . map fst . filter (uncurry (==)) . zip xs $ drop n (cycle xs)

solve1, solve2 :: [Int] -> Int
solve1 xs = captcha xs 1
solve2 xs = captcha xs (length xs `div` 2)

main :: IO ()
main = interact (show . solve2 . digits)
