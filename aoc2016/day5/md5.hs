#! /usr/bin/env stack
{-
   stack --resolver lts-10.1 --install-ghc runghc
    --package bytestring
    --package containers
    --package cryptohash-md5
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Data.Bits
import qualified Crypto.Hash.MD5 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Word as W

checkPrefix :: B.ByteString -> Bool
checkPrefix b
  | B.index b 0 == 0
  , B.index b 1 == 0
  , B.index b 2 < 16 = True
  | otherwise        = False

seed :: B.ByteString
seed = "ugkcyxxp"

ctx :: C.Ctx
ctx = C.update C.init seed

-- TODO: This could probably be optimized as `show` produces a String which
-- needs to be packed into a ByteString, leading to too many convertions.
numbers :: [B.ByteString]
numbers = L.map (BC.pack . show) [0..]

hashes :: [B.ByteString]
hashes = L.filter checkPrefix $ L.map (C.finalize . C.update ctx) numbers

toHex :: W.Word8 -> Char
toHex 0 = '0'; toHex 1 = '1'; toHex 2 = '2'; toHex 3 = '3'
toHex 4 = '4'; toHex 5 = '5'; toHex 6 = '6'; toHex 7 = '7'
toHex 8 = '8'; toHex 9 = '9'; toHex 10 = 'a'; toHex 11 = 'b'
toHex 12 = 'c'; toHex 13 = 'd'; toHex 14 = 'e'; toHex 15 = 'f'

solve1, solve2 :: String
solve1 = L.map (\h -> toHex $ B.index h 2) $ L.take 1 hashes

solve2 = L.map snd $ M.toAscList $ go M.empty hashes
  where
    go m (h:hs)
      | M.size m == 8 = m
      | otherwise =
        let n = B.index h 2
            v = toHex $ (B.index h 3 `shiftR` 4) .&. 15 in
          if n <= 7
             then go (M.alter
               (\case
                 Nothing -> Just v
                 j -> j) n m) hs
             else go m hs

main :: IO ()
main = print solve2
