import Control.Monad
import Data.Array

perm :: String -> String
perm s = let len = length s
             arr = listArray (0, len) s
             idx = concat. map (\(a, b) -> [b, a]) $ zip [0,2..len] [1,3..len]
    in map (\i -> arr ! i) idx

main :: IO ()
main = do
    n <- readLn
    forM_ [1..n] $ \i -> do
        l <- getLine
        putStrLn $ perm l
