
import qualified Data.IntMap.Strict as M
import qualified Data.List as L

data LList = LList [Int] Int [Int]
--                 ^     ^   ^ right
--                 |     | focused element
--                 | left

-- Perform one clockwise rotation on the list
clockwise :: LList -> LList
clockwise llist@(LList [] _ []) = llist
clockwise (LList left v (x:xs)) = LList (v:left) x xs
clockwise (LList left v []) = let (x:xs) = reverse left in LList [] x (xs ++ [v])

-- Perform one counter-clockwise rotation on the list
counterClockwise :: LList -> LList
counterClockwise llist@(LList [] _ []) = llist
counterClockwise (LList (x:xs) v right) = LList xs x (v:right)
counterClockwise (LList [] v right) = let (x:xs) = reverse right in LList (xs ++ [v]) x []

-- Perform 7 counter-clockwise rotations on the list
counterClockwise7 :: LList -> LList
counterClockwise7 = (
    counterClockwise
    . counterClockwise
    . counterClockwise
    . counterClockwise
    . counterClockwise
    . counterClockwise
    . counterClockwise)

-- Insert an element on the right of the currently focused element
insert :: LList -> Int -> LList
insert (LList left v right) value = LList (v:left) value right

-- Remove the currently focused element
remove :: LList -> LList
remove (LList left _ (x:xs)) = LList left x xs
remove (LList left _ []) = let (x:xs) = reverse left in LList [] x xs

-- Return currently focused element
focus :: LList -> Int
focus (LList _ v _) = v

solve :: Int -> Int -> Int
solve players lastMarble = go root 1 0 (M.fromList [(p, 0) | p <- [0..players-1]])
    where
        -- Initial marble
        root = LList [] 0 []
        -- Recursively play the game until maximum value is reached
        go :: LList -> Int -> Int -> M.IntMap Int -> Int
        go node currentMarble currentPlayer scores
          | currentMarble == lastMarble = L.maximum $ M.elems scores
          | (mod currentMarble 23) == 0 =
              let counterClockwiseShifted = counterClockwise7 node
                  updatedScores = (
                    M.adjust
                        (\v -> v + focus counterClockwiseShifted + currentMarble)
                        currentPlayer
                        scores)
                in go (remove counterClockwiseShifted) nextMarble nextPlayer updatedScores
          | otherwise =
                go (insert (clockwise node) currentMarble) nextMarble nextPlayer scores
            where
                nextPlayer = mod (currentPlayer + 1) players
                nextMarble = currentMarble + 1

main :: IO ()
main = print $ solve 435 7118400
-- main = print $ solve 435 71184
