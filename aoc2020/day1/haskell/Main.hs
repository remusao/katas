import Control.Monad (guard)

main:: IO ()
main = interact solve

solve :: String -> String
solve = show . head . go . map read . lines
  where
    go :: [Int] -> [Int]
    go lst = do
      let enumerated = zip [1..] lst
      (xi, x) <- enumerated
      (yi, y) <- drop xi enumerated
      guard (x + y < 2020)
      k <- drop yi lst
      guard (x + y + k == 2020)
      return (x * y * k)
