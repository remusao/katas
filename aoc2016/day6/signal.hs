#! /usr/bin/env stack
{-
   stack --resolver lts-10.1 --install-ghc runghc
    --package containers
-}

{-# LANGUAGE LambdaCase #-}

import qualified Data.List as L
import qualified Data.Map.Strict as M

frequency :: String -> M.Map Char Int
frequency = L.foldl
        (flip
           (M.alter
              (\case
                   Nothing -> Just 1
                   Just n -> Just (n + 1)))) M.empty

solve select =
  L.map (fst . select (\(_, n1) (_, n2) -> compare n1 n2) . M.toList . frequency) .
  L.transpose .
  L.lines

solve1, solve2 :: String -> String
solve1 = solve L.maximumBy
solve2 = solve L.minimumBy

main :: IO ()
main = interact (show . solve2)
