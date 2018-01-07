#! /usr/bin/env stack
-- stack --resolver lts-10.2 --install-ghc runghc --package bytestring

import Prelude hiding (head, tail, length, repeat)

import qualified Data.ByteString.Lazy.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Monoid as M

solve1 :: B.ByteString -> Int
solve1 = fromIntegral . B.length . BB.toLazyByteString . go mempty
  where
    go bb bs
      | B.null bs = bb
      | otherwise =
        let head = B.head bs
            tail = B.tail bs
        in
          if C.isSpace head then
            go bb tail
          else if head == '(' then
            let (marker, rest) = B.break (== ')') tail
                (length, r2) = B.break (== 'x') marker
                repeat = B.drop 1 r2
                (toRepeat, r3) = B.splitAt (read . B.unpack $ length) $ B.drop 1 rest
            in go (bb M.<> M.mconcat (L.replicate (read . B.unpack $ repeat) $ BB.lazyByteString toRepeat)) r3
          else
            go (bb M.<> BB.char7 head) tail

solve2 :: B.ByteString -> Int
solve2 = go
  where
    go :: B.ByteString -> Int
    go bs
      | B.null bs = 0
      | C.isSpace (B.head bs) = go (B.tail bs)
      | B.head bs /= '(' = 1 + go (B.tail bs)
      | otherwise = -- We got a '('
        let (marker, rest) = B.break (== ')') (B.tail bs)
            (length, r2) = B.break (== 'x') marker
            repeat = (read . B.unpack $ B.drop 1 r2) :: Int
            (toRepeat, r3) = B.splitAt (read . B.unpack $ length) $ B.drop 1 rest
         in repeat * go toRepeat + go r3

main :: IO ()
main = do
  input <- B.getContents
  putStrLn "Part1:"
  print $ solve1 input
  putStrLn "Part2:"
  print $ solve2 input
