
module Main (main) where

import Data.Array
import Data.Maybe

import Parser

data Instr = Nop Int | Jmp Int | Acc Int
  deriving Show

solve :: String -> String
solve input = show $ memo ! 0
  where
    program :: [Instr]
    program = fromMaybe [] $ parse (instruction `sepBy1` char '\n') input

    size :: Int
    size = length program

    instruction :: Parser Instr
    instruction = choice
      [ Nop <$> (string "nop " *> int)
      , Acc <$> (string "acc " *> int)
      , Jmp <$> (string "jmp " *> int)
      ]

    eval :: Int -> Instr -> Int
    eval offset (Jmp i) = get (offset + i)
    eval offset (Acc i) = i + get (offset + 1)
    eval offset _ = get (offset + 1)

    get :: Int -> Int
    get index = if index < size then memo ! index else 0

    memo :: Array Int Int
    memo = listArray (0, size) [eval idx instr | (idx, instr) <- zip [0..] program]

main :: IO ()
main = interact solve
