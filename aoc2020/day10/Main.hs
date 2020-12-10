{-# LANGUAGE Safe #-}

module Main (main) where

import Data.Array (Array, (!), listArray)
import Data.Ix (range)
import Data.List (sort)


-- Inspired by https://jelv.is/blog/Lazy-Dynamic-Programming/
solve2 :: [Int] -> Int
solve2 lst = memo ! (0, 0)
  where
    (idx_bound, curr_bound) = (length lst, maximum lst)

    bounds :: ((Int, Int), (Int, Int))
    bounds = ((0, 0), (idx_bound, curr_bound))

    arr :: Array Int Int
    arr = listArray (0, idx_bound) (sort lst)

    check :: Int -> Int -> Int
    check idx current
      | idx >= idx_bound = 0
      | n - current <= 3 = memo ! (idx + 1, n)
      | otherwise = 0
      where
        n = arr ! idx

    memo :: Array (Int, Int) Int
    memo = listArray bounds [
      if idx == idx_bound
        then 1
        else
          check (idx + 0) current +
          check (idx + 1) current +
          check (idx + 2) current
        | (idx, current) <- range bounds]


-- From: https://github.com/jonathanperret
-- solve2 :: [Int] -> Int
-- solve2 levels = (snd . last) ways
--   where
--     waysfor :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
--     waysfor (n3,c3) (n2,c2) (n1,c1) n =
--       (n,
--         (if n1 >= n-3 then c1 else 0) +
--         (if n2 >= n-3 then c2 else 0) +
--         (if n3 >= n-3 then c3 else 0))
--
--     ways :: [(Int, Int)]
--     ways = (-2,0) : (-1,0) : (0,1) :
--       zipWith4 waysfor
--         ways (drop 1 ways) (drop 2 ways)
--         (sort levels)


main :: IO ()
main = readFile "./input1.txt" >>= print . solve2 . map read . lines
