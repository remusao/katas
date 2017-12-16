compress :: String -> String
compress (h:t) = go t h 1
    where
        pair2str char 1 = [char]
        pair2str char count = [char] ++ (show count)
        go [] char count = pair2str char count
        go (e:t) char count
          | e == char = go t char (count + 1)
          | otherwise = (pair2str char count) ++ go t e 1

main :: IO ()
main = do
    l <- getLine
    putStrLn $ compress l
