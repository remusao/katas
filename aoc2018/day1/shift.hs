
import Data.List (cycle)
import Data.Char (digitToInt, isHexDigit)
import qualified Data.IntSet as S

unstack :: String -> Int
unstack = go 0 1
  where
    go acc _ [] = acc
    go acc decimal (c:xc) = go (acc + (decimal * digitToInt c)) (decimal * 10) xc

decode :: String -> [Int]
decode = reverse . go [] [] 1
  where
    go acc [] _ [] = acc
    go acc stack s [] = (s * unstack stack) : acc
    go acc stack s (c:xc)
      | c == '-' = go acc [] (-1) xc
      | c == '+' = go acc [] (1) xc
      | isHexDigit c = go acc (c : stack) s xc
      | c == '\n' || c == ',' = go ((s * unstack stack) : acc) [] 1 xc
      | otherwise = go acc stack s xc

solve1 :: String -> Int
solve1 = sum . decode

solve2 :: String -> Int
solve2 = (findDup 0 S.empty) . cycle. decode
  where
    findDup _ _ [] = undefined
    findDup acc s (x:xs) =
      let newAcc = acc + x in
          case S.member newAcc s of
            True -> newAcc
            False -> findDup newAcc (S.insert newAcc s) xs

main :: IO ()
-- Part 1
-- main = interact (show . solve1)
-- Part 2
main = interact (show . solve2)
