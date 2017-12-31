#! /usr/bin/env stack
-- stack --resolver lts-10.1 --install-ghc runghc --package containers

import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Set as S

data Instruction
  = L Int
  | R Int
  deriving Show

readInstructions :: String -> [Instruction]
readInstructions = go
  where
    go [] = []
    go (c:xs)
      | c == 'R' || c == 'L' =
        let
          (digit, rest) = L.span C.isDigit xs
          ctr =
            case c of
              'R' -> R
              'L' -> L
        in ctr (read digit) : go rest
      | otherwise = go xs

data Direction
  = North
  | West
  | South
  | East

rotR, rotL :: Direction -> Direction
rotR North = East
rotR East = South
rotR South = West
rotR West = North

rotL North = West
rotL West = South
rotL South = East
rotL East = North

newtype Position = Position (Int, Int)
  deriving (Show, Eq, Ord)

move :: Position -> Direction -> Int -> [Position]
move (Position (x, y)) direction distance =
  case direction of
    North -> [Position (x, y + n) | n <- [1..distance]]
    West -> [Position (x - n, y) | n <- [1..distance]]
    South -> [Position (x, y - n) | n <- [1..distance]]
    East -> [Position (x + n, y) | n <- [1..distance]]

follow :: [Instruction] -> [Position]
follow = go (Position (0, 0)) North
  where
    go c _ [] = [c]
    go c d (R n:xs) =
      let newDir = rotR d
          newPositions = move c newDir n
          newPos = last newPositions
       in newPositions ++ go newPos newDir xs
    go c d (L n:xs) =
      let newDir = rotL d
          newPositions = move c newDir n
          newPos = last newPositions
       in newPositions ++ go newPos newDir xs

solve1, solve2 :: [Instruction] -> Int
solve1 instructions =
  let Position (x, y) = last $ follow instructions
  in abs x + abs y

solve2 instructions = go S.empty (follow instructions)
  where
    go _ [] = -1
    go s (p@(Position (x, y)):xs)
      | p `S.member` s = abs x + abs y
      | otherwise = go (S.insert p s) xs

main :: IO ()
main = interact (show . solve2 . readInstructions)
