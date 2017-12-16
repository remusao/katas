f :: Int -> [Int] -> [Int]
f _ [] = []
f n (h:t) = replicate n h ++ f n t

main :: IO ()
main = getContents >>=
       mapM_ print. (\(n:arr) -> f n arr). map read. words
