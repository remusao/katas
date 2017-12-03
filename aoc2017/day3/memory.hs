#! /usr/bin/env stack
-- stack --resolver lts-9.14 --install-ghc runghc

mapAccu :: (t1 -> t -> t1) -> t1 -> [t] -> [t1]
mapAccu _ _ [] = []
mapAccu f accu (x:xs) =
  let newAccu = f accu x
   in newAccu : mapAccu f newAccu xs

spiral :: [(Int, (Int, Int))]
spiral = zip [2..] coords
  where
    start = (0, 0)
    coords = mapAccu (\(x, y) (dx, dy) -> (x + dx, y + dy)) start $ go 1
    go n =
      (1, 0) :
      replicate (2 * n - 1) (0, 1) ++
      replicate (2 * n)     (-1, 0) ++
      replicate (2 * n)     (0, -1) ++
      replicate (2 * n)     (1, 0) ++
      go (n + 1)

solve1 :: Int -> Maybe (Int, Int)
solve1 n = lookup n spiral

main :: IO ()
main = interact (show . solve1 . read)
