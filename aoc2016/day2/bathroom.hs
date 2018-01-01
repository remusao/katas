#! /usr/bin/env stack
-- stack --resolver lts-10.1 --install-ghc runghc

import qualified Data.List as L

data Instruction = U | L | R | D
  deriving (Show)

readInstructions :: String -> [[Instruction]]
readInstructions input = map go (lines input)
  where
    go [] = []
    go ('D':xs) = D : go xs
    go ('L':xs) = L : go xs
    go ('R':xs) = R : go xs
    go ('U':xs) = U : go xs
    go (_:xs) = go xs

data Digit
  = D1 | D2 | D3
  | D4 | D5 | D6
  | D7 | D8 | D9
  | DA | DB | DC
  | DD

digitsToString :: [Digit] -> String
digitsToString = map toDigit
  where
    toDigit D1 = '1'
    toDigit D2 = '2'
    toDigit D3 = '3'
    toDigit D4 = '4'
    toDigit D5 = '5'
    toDigit D6 = '6'
    toDigit D7 = '7'
    toDigit D8 = '8'
    toDigit D9 = '9'
    toDigit DA = 'A'
    toDigit DB = 'B'
    toDigit DC = 'C'
    toDigit DD = 'D'

solve :: (Digit -> Instruction -> Digit) -> [[Instruction]] -> String
solve move =
  digitsToString .
  L.tail .
  L.reverse .
  L.foldl' (\(d:ds) instrs -> L.foldl' move d instrs : d : ds) [D5]

solve1, solve2 :: [[Instruction]] -> String
solve1 = solve move
  where
    move :: Digit -> Instruction -> Digit
    move D1 U = D1; move D1 L = D1; move D1 R = D2; move D1 D = D4
    move D2 U = D2; move D2 L = D1; move D2 R = D3; move D2 D = D5
    move D3 U = D3; move D3 L = D2; move D3 R = D3; move D3 D = D6
    move D4 U = D1; move D4 L = D4; move D4 R = D5; move D4 D = D7
    move D5 U = D2; move D5 L = D4; move D5 R = D6; move D5 D = D8
    move D6 U = D3; move D6 L = D5; move D6 R = D6; move D6 D = D9
    move D7 U = D4; move D7 L = D7; move D7 R = D8; move D7 D = D7
    move D8 U = D5; move D8 L = D7; move D8 R = D9; move D8 D = D8
    move D9 U = D6; move D9 L = D8; move D9 R = D9; move D9 D = D9
    -- That should never happen
    move d _ = d

solve2 = solve move
  where
    move :: Digit -> Instruction -> Digit
    move D1 D = D3; move D1 _ = D1
    move D2 R = D3; move D2 D = D6; move D2 _ = D2
    move D3 L = D2; move D3 U = D1; move D3 R = D4; move D3 D = D7
    move D4 L = D3; move D4 D = D8; move D4 _ = D4

    move D5 R = D6; move D5 _ = D5
    move D6 L = D5; move D6 R = D7; move D6 U = D2; move D6 D = DA
    move D7 L = D6; move D7 R = D8; move D7 U = D3; move D7 D = DB
    move D8 L = D7; move D8 R = D9; move D8 U = D4; move D8 D = DC
    move D9 L = D8; move D9 _ = D9

    move DA R = DB; move DA U = D6; move DA _ = DA
    move DB L = DA; move DB R = DC; move DB D = DD; move DB U = D7
    move DC L = DB; move DC U = D8; move DC _ = DC
    move DD U = DB; move DD _ = DD

main :: IO ()
main = interact (solve2 . readInstructions)
