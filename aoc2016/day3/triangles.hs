#! /usr/bin/env stack
-- stack --resolver lts-10.1 --install-ghc runghc --package containers

import qualified Data.Maybe as M
import qualified Data.List as L

newtype Triangle = Triangle (Int, Int, Int)
  deriving Show

solve1 :: String -> Int
solve1 = length . readTriangles
  where
    readTriangles :: String -> [Triangle]
    readTriangles = M.mapMaybe (mkTriangle . map read . words) . lines
      where
        mkTriangle :: [Int] -> Maybe Triangle
        mkTriangle [a, b, c]
          | a + b > c
          , a + c > b
          , b + c > a = Just $ Triangle (a, b, c)
          | otherwise = Nothing
        mkTriangle _ = Nothing

solve2 :: String -> Int
solve2 input = length $ go coords
  where
    coords = L.concat . L.transpose . L.map (map read . words) . lines $ input
    go (a:b:c:xs)
      | a + b > c
      , a + c > b
      , b + c > a = Triangle (a, b, c) : go xs
      | otherwise = go xs
    go _ = []

main :: IO ()
main = interact (show . solve2)
