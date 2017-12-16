import Control.Applicative

fibonacci :: Integer -> Integer -> [Integer]
fibonacci a b = a : b : zipWith (\a b -> a + b * b) (fibonacci a b) (tail (fibonacci a b))

main :: IO ()
main = do
    [a, b, n] <- words <$> getLine
    let aI = read a :: Integer
    let bI = read b :: Integer
    let nI = read n :: Int
    print $ (fibonacci aI bI) !! (nI - 1)
