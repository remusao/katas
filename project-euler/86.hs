

dist :: Float -> Float -> Float -> Float
dist a b c = sqrt(a * a + (b + c) * (b + c))

isInt :: Float -> Bool
isInt d = floor d == ceiling d

main :: IO ()
main = print . head . drop 100000 $ do
    a <- [1..]
    b <- [1..a]
    c <- [1..b]
    if isInt $ dist a b c then [a] else []
