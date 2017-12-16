import Control.Applicative
import Data.List (sort)
import qualified Data.Array as A
import Data.Ix (range)
import qualified Data.Vector as V

getInts :: IO [Int]
getInts = fmap read . words <$> getLine

solve :: V.Vector Int -> Int -> Int
solve coins n = go n 0
    where
        go :: Int -> Int -> Int
        go 0 _ = 1
        go n i = sum . map access_memo . range $ (i, number_of_coins - 1)
            where
                access_memo index =
                    let new_n = n - (coins V.! index)
                    in if new_n >= 0
                    then memo A.! (new_n, index)
                    else 0

        -- Constants
        number_of_coins = V.length coins
        memo = A.listArray bounds [go i j | (i, j) <- range bounds]
        bounds = ((0, 0), (n, V.length coins - 1)) :: ((Int, Int), (Int, Int))

main :: IO ()
main = do
    [n, _] <- getInts
    coins <- getInts
    let sorted_coins = (V.reverse . V.fromList . sort) coins
    print $ solve sorted_coins n
