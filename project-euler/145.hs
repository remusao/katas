
bound :: Int
bound = 1000000000

rev :: Int -> Int
rev = go 0
    where
        go r 0 = r
        go r n = let (d, m) = n `divMod` 10
                 in go (r * 10 + m) d

solve :: Int
solve = go bound 0
    where
        reversible :: Int -> Bool
        reversible n
            | n == 0            = True
            | n `mod` 10 == 0   = False
            | otherwise         = let (d, m) = n `divMod` 10
                                  in (odd m) && reversible d
        go :: Int -> Int -> Int
        go 0 c = c
        go n c = let r = rev n
                     incr = case (n `mod` 10 /= 0 && r `mod` 10 /= 0) of
                        True -> if reversible (r + n) then 1 else 0
                        False -> 0
                in go (n - 1) (c + incr)

main :: IO ()
main = print solve
