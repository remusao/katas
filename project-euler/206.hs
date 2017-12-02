

check :: Int -> Bool
check = go 1234567890
    where
        go 0 0 = True
        go _ 0 = False
        go d i = let d0 = mod i 10
                     i0 = div i 100
                 in d0 == d `mod` 10 && go (d `div` 10) i0


solve :: Int
solve = go $ round $ sqrt 1020304050607080900
    where
        go i
            | check (i * i) = i
            | otherwise = go $ i + 10


main :: IO ()
main = print solve
