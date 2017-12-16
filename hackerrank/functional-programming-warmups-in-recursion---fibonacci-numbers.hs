module Main where

fib :: Int -> Int
fib n = fibo !! (n - 1)
    where
        fibo = 0 : 1 : zipWith (+) fibo (tail fibo)

main = do
    input <- getLine
    print . fib . (read :: String -> Int) $ input
