{-# LANGUAGE Safe #-}

module Main (main) where

import Data.List (sort, insert)

-- | Poor man's queue with just minimal operations
data Queue = Queue [Int] [Int]

-- | Push element at the end of the queue.
push :: Int -> Queue -> Queue
push i (Queue h l) = Queue h (i:l)

-- | Pop first element from queue.
pop :: Queue -> (Int, Queue)
pop (Queue (x:xs) l) = (x, Queue xs l)
pop (Queue _ l) = let (x:xs) = reverse l in (x, Queue xs [])


solve1 :: Int -> [Int] -> Int
solve1 preambleSize l = go (Queue preamble []) (sort preamble) rest
  where
    (preamble, rest) = splitAt preambleSize l

    go :: Queue -> [Int] -> [Int] -> Int
    go q p (x:xs)
      | not (f x p (reverse p)) = x
      | otherwise =
        let (q2, qs2) = pop q
         in go (push x qs2) (insert x . filter (/=q2) $ p) xs

    -- | Try to find `x` and `y` which sum to `n` in linear time
    --   Return `True` if found and `False` otherwise.
    f :: Int -> [Int] -> [Int] -> Bool
    f n (x:xs) (y:ys)
      | x == y = False
      | otherwise =
        case compare (x + y) n of
          EQ -> True
          LT -> f n xs (y:ys)
          GT -> f n (x:xs) ys


solve2 :: Int -> [Int] -> Int
solve2 n l = go (head l) (0, 1) l (drop 1 l)
  where
    go :: Int -> (Int, Int) -> [Int] -> [Int] -> Int
    go acc (lo, hi) (x:xs) (y:ys)
      | acc == n =
        let slice = take (hi - lo) (drop lo l)
         in maximum slice + minimum slice
      | otherwise =
        case compare acc n of
          LT -> go (acc + y) (lo, hi + 1) (x:xs) ys
          GT -> go (acc - x) (lo + 1, hi) xs (y:ys)


main :: IO ()
main = do
  inputs <- readFile "./src/input1.txt"
  let numbers = map read (lines inputs) :: [Int]
  let p1 = solve1 25 numbers
  let p2 = solve2 p1 numbers
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
