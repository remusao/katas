solve :: Double -> Double
solve x = go 0 1.0 1.0
    where
        go i pow fact
            | i >= 10    = 0
            | otherwise = (pow / fact) + go (i + 1) (pow * x) (fact * (i + 1))

main :: IO ()
main = getContents >>= mapM_ print. map solve. map (read::String->Double). tail. words
