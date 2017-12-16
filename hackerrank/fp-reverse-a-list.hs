rev :: [a] -> [a]
rev = foldl (\l r -> r : l) []

main = do
		inputdata <- getContents
		mapM_ putStrLn $ map show $ rev $ map (read :: String -> Int) $ lines inputdata
