len :: [a] -> Int
len = foldr (\_ c -> c + 1) 0

main = do
		inputdata <- getContents
		putStrLn $ show $ len $ map (read :: String -> Int) $ lines inputdata
