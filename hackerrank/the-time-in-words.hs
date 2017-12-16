import Control.Applicative
import Control.Monad
import System.IO

readInt :: IO Int
readInt = readLn

solve :: Int -> Int -> String
solve h m
    | m == 0    = hour ++ " o' clock"
    | m == 15   = "quarter past " ++ hour
    | m == 45   = "quarter to " ++ (convert (h + 1))
    | m == 30   = "half past " ++ hour
    | m < 30    = minutes ++ " minutes past " ++ hour
    | otherwise = minutes ++ " minutes to " ++ (convert (h + 1))
    where
        hour  = convert h
        minutes = convert m

main :: IO ()
main = do
    h <- readInt
    m <- readInt
    putStr $ solve h m

convert :: Int -> String
convert 1 = "one"
convert 2 = "two"
convert 3 = "three"
convert 4 = "four"
convert 5 = "five"
convert 6 = "six"
convert 7 = "seven"
convert 8 = "eight"
convert 10 = "ten"
convert 11 = "eleven"
convert 12 = "twelve"
convert 13 = "thirteen"
convert 14 = "fourteen"
convert 15 = "fiveteen"
convert 16 = "sixteen"
convert 17 = "seventeen"
convert 18 = "eighteen"
convert 19 = "nineteen"
convert 20 = "twenty"
convert 21 = "twenty one"
convert 22 = "twenty two"
convert 23 = "twenty three"
convert 24 = "twenty four"
convert 25 = "twenty five"
convert 26 = "twenty six"
convert 27 = "twenty seven"
convert 28 = "twenty eight"
convert 29 = "twenty nine"
convert m = convert (60 - m)
