{-# LANGUAGE Safe #-}

module Main (main) where

import Data.Maybe (mapMaybe)
import Control.Applicative ((<|>))
import Data.Functor (($>))

import Parser

main :: IO ()
main = interact $ show . solve . lines

solve :: [String] -> (Int, Int)
solve inputs = (go part1, go part2)
  where
    go :: Parser Int -> Int
    go p = sum $ mapMaybe (parse p) inputs

plus :: Parser (Int -> Int -> Int)
plus = string " + " $> (+)

times :: Parser (Int -> Int -> Int)
times = string " * " $> (*)

part1 :: Parser Int
part1 = chainl1 (int <|> paren part1) (plus <|> times)

part2 :: Parser Int
part2 = chainl1 (chainl1 (int <|> paren part2) plus) times

paren :: Parser a -> Parser a
paren p = char '(' *> p <* char ')'
