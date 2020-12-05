module Main (main) where

int64 :: String -> Int
int64 = foldl (
    \acc c -> 2 * acc + case c of
      'B' -> 1
      'R' -> 1
      _ -> 0
  ) 0

solve :: String -> (Int, Int)
solve input = (max_id, sum [min_id..max_id] - sum_id)
  where
    ids = map int64 . lines $ input
    min_id = minimum ids
    max_id = maximum ids
    sum_id = sum ids

main :: IO ()
main = interact $ show . solve
