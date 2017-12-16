mingle :: String -> String -> String
mingle l1 l2 = concat . map (\(a, b) -> [a, b]) $ zip l1 l2

main :: IO ()
main = do
    l1 <- getLine
    l2 <- getLine
    putStrLn $ mingle l1 l2
