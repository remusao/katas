#! /usr/bin/env stack
-- stack --resolver lts-10.2 --install-ghc runghc --package containers

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Set as S

newtype Col = Col Int deriving (Show, Ord, Eq, Num)
newtype Row = Row Int deriving (Show, Ord, Eq, Num)

data Op
  = Rect Int Int
  | RotRow Row Int
  | RotCol Col Int
  deriving (Show, Ord, Eq)

parse :: String -> [Op]
parse = M.mapMaybe go . L.lines
  where
    isRect = ("rect " `L.isPrefixOf`)
    isRotRow = ("rotate row " `L.isPrefixOf`)
    isRotCol = ("rotate column " `L.isPrefixOf`)
    go line
      | isRect line = do
        -- rect AxB
        let op = L.drop 5 line
        x <- L.elemIndex 'x' op
        let a = read $ L.take x op
        let b = read $ L.drop (x + 1) op
        return $ Rect a b
      | isRotRow line = do
        -- rotate row y=A by B
        let op = L.drop 13 line
        let (a, rest) = L.break (' ' ==) op
        let b = L.drop 4 rest
        return $ RotRow (Row $ read a) (read b)
      | isRotCol line = do
        -- rotate column x=A by B
        let op = L.drop 16 line
        let (a, rest) = L.break (' ' ==) op
        let b = L.drop 4 rest
        return $ RotCol (Col $ read a) (read b)
      | otherwise = Nothing

run :: [Op] -> S.Set (Col, Row)
run = go S.empty
  where
    go s [] = s
    go s (op:ops) =
      case op of
        Rect a b ->
          go (s `S.union` S.fromList [
            (Col x, Row y)
              | x <- [0..a - 1]
              , y <- [0..b - 1] ]) ops
        RotRow row b ->
          go (s
            `S.difference` S.fromList [
              (Col x, row)
                | x <- [0..49] ]
            `S.union` S.fromList [
              (Col $ (x + b) `mod` 50, row)
                | x <- [0..49],
                (Col x, row) `S.member` s ]
            ) ops
        RotCol col b ->
          go (s
            `S.difference` S.fromList [
              (col, Row y)
                | y <- [0..49] ]
            `S.union` S.fromList [
              (col, Row $ (y + b) `mod` 6)
                | y <- [0..5],
                (col, Row y) `S.member` s]
            ) ops

solve1 :: [Op] -> Int
solve1 = S.size . run

solve2 :: [Op] -> String
solve2 ops = L.unlines [
  [ if (Col x, Row y) `S.member` coords then '#' else '.' | x <- [0..59] ]
    | y <- [0..5] ]
  where
    coords = run ops

main :: IO ()
main = interact (solve2 . parse)
