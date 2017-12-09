#! /usr/bin/env stack
-- stack --resolver lts-9.14 --install-ghc runghc

solve2 :: String -> Int
solve2 stream = length $ go stream False
  where
    go :: String -> Bool -> String
    go [] _ = []
    go ('!':_:xs) g = go xs g
    go ('<':xs) g
      | g = '<' : go xs g
      | otherwise = go xs True
    go ('>':xs) _ = go xs False
    go ('{':xs) g
      | g = '{' : go xs g
      | otherwise = go xs g
    go ('}':xs) g
      | g = '}' : go xs g
      | otherwise = go xs g
    go (x:xs) g
      | g = x : go xs g
      | otherwise = go xs g

solve1 :: String -> Int
solve1 stream = sum $ go stream 1 False
  where
    go :: String -> Int -> Bool -> [Int]
    go [] _ _ = []
    go ('!':_:xs) d g = go xs d g
    go ('<':xs) d g
      | g = go xs d g
      | otherwise = go xs d True
    go ('>':xs) d _ = go xs d False
    go ('{':xs) d g
      | g = go xs d g
      | otherwise = d : go xs (d + 1) g
    go ('}':xs) d g
      | g = go xs d g
      | otherwise = go xs (d - 1) g
    go (_:xs) d g = go xs d g


main :: IO ()
main = interact (show . solve2)
