
module Main where

import Control.Applicative

solve :: String -> String
solve s = let (a:b:c:[]) = (map read $ words s) :: [Int] in
    case a + 2 * (b - a) == c of
        True -> "AP " ++ (show $ c + (b - a))
        False -> "GP " ++ (show $ c * (quot b a))

main :: IO ()
main = (join "\n") . (map solve) . init . lines <$> getContents >>= putStrLn
    where
        join _ [] = []
        join sep (h:t) = foldl (\a b -> a ++ sep ++ b) h t
