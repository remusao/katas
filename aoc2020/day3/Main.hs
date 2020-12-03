{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Prelude ()

import GHC.Num ((*), (+))
import GHC.Show (show)

import System.IO (IO, interact)

import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Int (Int)
import Data.List ((++), splitAt, product, map, cycle, lines)
import Data.String (String)


countTrees :: (Int, Int) -> [String] -> Int
countTrees (slopeR, slopeC) = go 0 1
  where
    go :: Int -> Int -> [String] -> Int
    go count idx rows =
      case splitAt slopeR rows of
        (_, r:rs) ->
          case splitAt (idx * slopeC) r of
            (_, c:_) ->
              go
                (if c == '#' then count + 1 else count)
                (idx + 1)
                (r:rs)
            _ -> count
        _ -> count


main :: IO ()
main = interact go
  where
    solve :: [String] -> [(Int, Int)] -> Int
    solve rows = product . map (`countTrees` rows)

    go :: String -> String
    go input =
      let
        rows = map cycle . lines $ input
        solution1 = solve rows [(1, 3)]
        solution2 = solution1 * solve rows [(1, 1), (1, 5), (1, 7), (2, 1)]
      in ("Part1: " ++ show solution1 ++ "\nPart2: " ++ show solution2)
