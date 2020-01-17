#! /usr/bin/env stack
{-
stack --resolver lts-9.17 --install-ghc runghc --package megaparsec
-}

import Debug.Trace
import Data.Maybe
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String


data Program = Program
  { name :: String
  , weight :: Int
  , children :: [String]
  } deriving (Show, Read, Eq)

-- Comparison on `weight`
instance Ord Program where
  compare p1 p2 = compare (weight p1) (weight p2)

solve1 :: [Program] -> Int
solve1 = length

parsePrograms :: Parser [Program]
parsePrograms = many program
  where
    program = do
      name <- some alphaNumChar
      weight <- space >> char '(' *> some digitChar <* char ')'
      children <- try parseChildren <|> return []
      eol
      return Program {
        name = name,
        weight = read weight,
        children = children
      }
    parseChildren = space >> string "->" >> space *> (some alphaNumChar `sepBy` string ", ")

loadPrograms :: String -> [Program]
loadPrograms input = fromMaybe [] (parseMaybe parsePrograms input)

main :: IO ()
main = interact (show . solve1 . loadPrograms)
