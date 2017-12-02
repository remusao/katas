

import qualified Data.Numbers.Primes as P
import qualified Data.Set as S
import qualified Data.Array as A


intLog10' :: Int -> Int
intLog10' n
  | n < 10          = 1
  | n < 100         = 2
  | n < 1000        = 3
  | n < 10000       = 4
  | n < 100000      = 5
  | n < 1000000     = 6
  | n < 10000000    = 7
  | n < 100000000   = 8
  | n < 1000000000  = 9
  | n < 10000000000 = 10

maxprime :: Int
maxprime = 10000

concatInt :: Int -> Int -> Int
concatInt a b = a * (10 ^ (intLog10' b)) + b

primes' :: [Int]
primes' = takeWhile (< maxprime) P.primes

primeset :: S.Set Int
primeset = S.fromList primes'

icp :: Int -> Int -> Bool
icp a b = S.member (concatInt a b) primeset
       && S.member (concatInt b a) primeset

coprimes :: A.Array (Int, Int) Bool
coprimes = A.array bound [((i, j), isCoPrime i j) | (i, j) <- A.range bound]
    where
        bound = ((2, 2), (maxprime, maxprime))
        isCoPrime i j
            | j < i     = icp i j
            | i >= j    = coprimes A.! (j, i)
            | otherwise = False

solve :: [[Int]]
solve = do
    p1 <- primes'
    let t1 = f p1 $ takeWhile (< p1) primes'
    p2 <- t1
    let t2 = f p2 $ takeWhile (< p2) t1
    p3 <- t2
    let t3 = f p3 $ takeWhile (< p3) t2
    p4 <- t3
    return [p1, p2, p3, p4]
    where
        f x = filter (\y -> coprimes A.! (x, y))

main :: IO ()
main = print $ 42
