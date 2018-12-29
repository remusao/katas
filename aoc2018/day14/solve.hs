import qualified Data.Sequence as S
import Data.Sequence (fromList, (|>))

generate :: [Int]
generate = 3 : 7 : go (fromList [3, 7])0  1
    where
        go recipes pos1 pos2 =
            if q == 0
               then r :
                 go
                    (recipes |> r)
                    ((pos1 + r1 + 1) `rem` (S.length recipes + 1))
                    ((pos2 + r2 + 1) `rem` (S.length recipes + 1))
               else q : r :
                 go
                    (recipes |> q |> r)
                    ((pos1 + r1 + 1) `rem` (S.length recipes + 2))
                    ((pos2 + r2 + 1) `rem` (S.length recipes + 2))
            where
                r1 = S.index recipes pos1
                r2 = S.index recipes pos2
                (q, r) = (r1 + r2) `quotRem` 10

part1 :: String
part1 = concatMap show . take 10 $ drop 580741 generate

part2 :: Int
part2 = go 0 generate
    where
        go n (5:8:0:7:4:1:_) = n
        go n (_:xs) = go (n + 1) xs
        go _ _ = undefined

main :: IO ()
main = print (part1, part2)
