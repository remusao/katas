import Control.Monad

readInt :: IO Int
readInt = readLn

solve :: Int -> Int
solve 0 = 0
solve n = go n
    where
        go 0 = 0
        go x = let digit = x `mod` 10
                   remaining = x `div` 10
            in if digit > 0 && (n `rem` digit) == 0
                 then 1 + go remaining
                 else go remaining

main :: IO ()
main = do
   t <- readInt
   forM_ [1..t] $ \_ -> do
       n <- readInt
       print $ solve n
