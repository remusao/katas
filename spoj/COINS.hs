
module Main where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Data.Map (Map, empty, insert, lookup)

mlookup ::Integer -> Map Integer Integer -> (Integer, Map Integer Integer)
mlookup n m =
    case lookup n m of
        Nothing -> let (newVal, m1) = amount n m in (newVal, insert n newVal m1)
        Just a -> (a, m)


amount :: Integer -> Map Integer Integer -> (Integer, Map Integer Integer)
amount n m
    | n < 12 = (n, m)
    | otherwise =
        let (a, m1) = mlookup (quot n 3) m
            (b, m2) = mlookup (quot n 2) m1
            (c, m3) = mlookup (quot n 4) m2
            res = a + b + c in
            if res > n then (res, m3) else (n, m3)


solve :: String -> Map Integer Integer -> (Integer, Map Integer Integer)
solve s m = let n = (read s) :: Integer in
    amount n m


main :: IO ()
main = (join "\n") . (solveMemo (empty :: Map Integer Integer)) . lines <$> getContents >>= putStrLn
    where
        solveMemo :: Map Integer Integer -> [String] -> [String]
        solveMemo m (h:[]) = (show $ fst $ solve h m) : []
        solveMemo m (h:t) = let (ans, m1) = solve h m in
                            (show ans) : (solveMemo m1 t)
        join _ [] = []
        join sep (h:t) = foldl (\a b -> a ++ sep ++ b) h t
