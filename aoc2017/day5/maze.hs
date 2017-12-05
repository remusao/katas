

data Maze = Maze [Int] Int [Int]

maze :: [Int] -> Maze
maze (x:xs) = Maze [] x xs

left :: Maze -> Maybe Maze
left (Maze [] _ _) = Nothing
left (Maze (l:ls) v r) = Just $ Maze ls l (v:r)

right :: Maze -> Maybe Maze
right (Maze _ _ []) = Nothing
right (Maze l v (r:rs)) = Just $ Maze (v:l) r rs

incr :: Maze -> Maze
incr (Maze l v r) = Maze l (v + 1) r

try :: Int -> Maze -> (Maze -> Maybe Maze) -> Maybe Maze
try 0 m _ = Just m
try n m move =
  case move m of
    Nothing -> Nothing
    Just m2 -> try (n - 1) m2 move

solve :: [Int] -> (Maze -> Maze) -> Int
solve jumps update = go (maze jumps) 0
  where
    go m@(Maze _ v _) accu =
      let next =
            case compare v 0 of
              LT -> try (abs v) (update m) left
              EQ -> Just (update m)
              GT -> try v (update m) right
      in
        case next of
          Nothing -> accu + 1
          Just m2 -> go m2 (accu + 1)

solve1, solve2 :: [Int] -> Int
solve1 jumps = solve jumps incr
solve2 jumps = solve jumps update2
  where
    update2 (Maze l v r)
      | v >= 3 = Maze l (v - 1) r
      | otherwise = Maze l (v + 1) r

main :: IO ()
main = interact (show . solve2 . map read . lines)
