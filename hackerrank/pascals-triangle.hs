import Control.Monad

facto :: Int -> Int
facto k = product [1..k]

line :: Int -> String
line i = unwords . map (show . coeff) $ [0..i]
    where
        coeff j = let ne = facto i
                      re = facto j
                      nre = facto (i - j)
                  in div ne (re * nre)

main :: IO ()
main = do
    k <- readLn
    forM_ [0..k-1] $ \i ->
        putStrLn $ line i
