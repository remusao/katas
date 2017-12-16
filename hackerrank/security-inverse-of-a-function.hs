import Control.Monad
import Control.Applicative
import Data.List

main :: IO ()
main = do
    n <- readLn :: IO Int
    xs <- (fmap read <$> words <$> getLine) :: IO [Int]
    putStrLn $ unlines $ map (show . snd) $ sortBy (\(x1, _) (x2, _) -> compare x1 x2) $ zip xs [1..]
