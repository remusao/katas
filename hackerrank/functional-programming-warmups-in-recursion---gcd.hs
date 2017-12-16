module Main where

gcd' :: Integral a => a -> a -> a
gcd' n m
    | n == m    = n
    | m > n     = gcd' m n
    | otherwise = gcd' (n - m) m -- Complete this function

main :: IO ()
main = do
  input <- getLine
  print . uncurry gcd' . listToTuple . convertToInt . words $ input
 where
  listToTuple (x:xs:_) = (x,xs)
  convertToInt = map (read :: String -> Int)
