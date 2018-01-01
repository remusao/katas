#! /usr/bin/env stack
{-
   stack --resolver lts-10.1 --install-ghc runghc --package text
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Room = Room
  { name :: T.Text
  , sector :: Int
  , checksum :: T.Text
  } deriving Show

readRooms :: T.Text -> [Room]
readRooms input = map mkRoom $ T.lines input
  where
    mkRoom line =
      let chunks = T.splitOn "-" line
          [s, c] = T.splitOn "[" . T.dropEnd 1 . L.last $ chunks
          name = T.concat . L.init $ chunks
       in Room { name = name, sector = read (T.unpack s), checksum = c }

mkCounter :: Num a => T.Text -> M.Map Char a
mkCounter = T.foldl' (flip $ M.alter update) M.empty
  where
    update Nothing = Just 1
    update (Just v) = Just (v + 1)

mkChecksum :: (Fractional b, Ord b) => M.Map Char b -> T.Text
mkChecksum counter = T.pack $ L.take 5 $ L.map fst $ L.sortOn (\(c, v) -> (1 / v, c)) $ M.toList counter

isValid :: Room -> Bool
isValid Room{ name, checksum } = checksum == mkChecksum (mkCounter name)

solve1 :: [Room] -> Int
solve1 = L.sum . L.map sector . L.filter isValid

solve2 :: [Room] -> [Room]
solve2 = L.filter (\Room { name } -> "object" `T.isInfixOf` name) . L.map decrypt . L.filter isValid
  where
    decipher n c = C.chr $ (C.ord c - C.ord 'a' + n) `mod` 26 + C.ord 'a'
    decrypt r@Room{ name, sector } = r{ name = T.map (decipher sector) name }

main :: IO ()
main = TIO.interact (T.pack . show . solve2 . readRooms)
