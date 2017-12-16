{-# LANGUAGE ScopedTypeVariables #-}

facto :: Integer -> Integer
facto 0 = 1
facto 1 = 1
facto 2 = 2
facto n = n * facto (n - 1)

main :: IO ()
main = do
    n :: Integer <- fmap read getLine
    print $ facto n
