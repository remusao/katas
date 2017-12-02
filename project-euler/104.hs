
import Math.NumberTheory.Logarithms
import Data.Bits


minPandigital :: Int
minPandigital = 123456789

pandigital :: Int -> Bool
pandigital i
    | i < minPandigital = False
    | otherwise         = 1022 == go i
    where
        go :: Int -> Int
        go 0 = 0
        go n = go (n `div` 10) `setBit` (n `mod` 10)

fibo :: [Integer]
fibo = 1 : 1 : zipWith (+) fibo (tail fibo)

fibi :: [(Int, Integer)]
fibi = zip [1..] fibo

lowerbound :: Integer
lowerbound = 10^500

check :: Integer -> Bool
check n
    | n <= lowerbound = False
    | otherwise = rpand && lpand
    where
        rpand = pandigital (fromIntegral (n `mod` 10^9) :: Int)
        lpand = let ndigits = integerLog10' n
                    leftDigits = fromIntegral (n `div` fromIntegral 10 ^ (ndigits - 8)) :: Int
            in pandigital leftDigits

solve :: [(Int, Integer)] -> Int
solve lst = fst . head $ filter (\(_, n) -> check n) lst

main :: IO ()
main = print $ solve fibi
