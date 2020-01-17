#! /usr/bin/env stack
-- stack --resolver lts-9.14 --install-ghc runghc


import qualified Data.Map as M

data Direction =
    N
  | S
  | NE
  | SW
  | NW
  | SE
  deriving (Show)

simplify :: Direction -> Direction -> Maybe Direction
simplify N S = Nothing
simplify NE SW = Nothing
simplify NW SE = Nothing
simplify NE S = Just SE
simplify N SE = Just NE
simplify NW NE = Just N
simplify S NW = Just SW
simplify SE SW = Just S
simplify _ _ =

loadDirections :: String -> [Direction]
loadDirections _ = []

solve1 :: String -> Int
solve1 = length . loadDirections

main :: IO ()
main = interact (show . solve1)
