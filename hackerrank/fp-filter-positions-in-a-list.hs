f :: [Int] -> [Int]
f lst = go 0 lst
    where
        go _ [] = []
        go i (h:t)
            | i `mod` 2 == 0 = go (i + 1) t
            | otherwise  = h : go (i + 1) t

main = do
   inputdata <- getContents
   mapM_ (putStrLn. show). f. map read. lines $ inputdata
