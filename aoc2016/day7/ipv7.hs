#! /usr/bin/env stack
-- stack --resolver lts-10.2 --install-ghc runghc --package containers

import qualified Data.List as L

solve1 :: String -> Int
solve1 = L.length . L.filter (tls False False) . L.lines
  where
    tls _ abba ('[':xs) = tls True abba xs
    tls _ abba (']':xs) = tls False abba xs
    tls bracket abba (c1:c2:c3:c4:xs)
      | c1 == c4
      , c2 == c3
      , c1 /= c2 = not bracket && tls bracket True xs
      | otherwise = tls bracket abba (c2:c3:c4:xs)
    tls _ abba _ = abba

data ABA = ABA Char Char deriving Eq
data BAB = BAB Char Char deriving Eq

solve2 :: String -> Int
solve2 = L.length . L.filter ssl . L.lines
  where
    ssl :: String -> Bool
    ssl line =
      let (abas, babs) = collect line
       in L.any (\(ABA c1 c2) -> BAB c2 c1 `L.elem` babs) abas
    collect = go False [] []
      where
        go _ abas babs ('[':xs) = go True abas babs xs
        go _ abas babs (']':xs) = go False abas babs xs
        go bracket abas babs (c1:c2:c3:xs)
          | c1 == c3
          , not bracket = go bracket (ABA c1 c2 : abas) babs (c2:c3:xs)
          | c1 == c3
          , bracket = go bracket abas (BAB c1 c2 : babs) (c2:c3:xs)
          | otherwise = go bracket abas babs (c2:c3:xs)
        go _ abas babs _ = (abas, babs)

main :: IO ()
main = interact (show . solve2)
