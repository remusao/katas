{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE Strict #-}

{- Solution for the following puzzle, using the List Monad: https://en.m.wikipedia.org/wiki/Zebra_Puzzle -}

module Main (main) where

import Control.Monad (guard)
import Data.List (permutations)

data Color
  = Red
  | Green
  | Ivory
  | Blue
  | Yellow
  deriving stock (Show, Eq)

data Nationality
  = Englishman
  | Spaniard
  | Ukrainian
  | Norwegian
  | Japanese
  deriving stock (Show, Eq)

data Pet
  = Dog
  | Fox
  | Horse
  | Zebra
  | Snails
  deriving stock (Show, Eq)

data Drink
  = Tea
  | Coffee
  | Milk
  | OrangeJuice
  | Water
  deriving stock (Show, Eq)

data Smoke
  = OldGold
  | Kools
  | Chesterfield
  | Parliament
  | LuckyStrike
  deriving stock (Show, Eq)

newtype House = House (Color, Nationality, Pet, Drink, Smoke) deriving stock (Show)

newtype Solution = Solution (House, House, House, House, House) deriving stock (Show)

main :: IO ()
main =
  putStrLn
    ( "The "
        ++ show (drinksWater solution)
        ++ " drinks water, and the "
        ++ show (ownsZebra solution)
        ++ " owns a zebra!"
    )
  where
    drinksWater :: Solution -> Nationality
    drinksWater
      ( Solution
          ( House (_, n1, _, d1, _),
            House (_, n2, _, d2, _),
            House (_, n3, _, d3, _),
            House (_, n4, _, d4, _),
            House (_, n5, _, d5, _)
            )
        )
        | d1 == Water = n1
        | d2 == Water = n2
        | d3 == Water = n3
        | d4 == Water = n4
        | d5 == Water = n5
        | otherwise = undefined

    ownsZebra :: Solution -> Nationality
    ownsZebra
      ( Solution
          ( House (_, n1, p1, _, _),
            House (_, n2, p2, _, _),
            House (_, n3, p3, _, _),
            House (_, n4, p4, _, _),
            House (_, n5, p5, _, _)
            )
        )
        | p1 == Zebra = n1
        | p2 == Zebra = n2
        | p3 == Zebra = n3
        | p4 == Zebra = n4
        | p5 == Zebra = n5
        | otherwise = undefined

    solution :: Solution
    solution = head $ do
      [n1, n2, n3, n4, n5] <- permutations [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]

      -- 10. The Norwegian lives in the first house.
      guard (n1 == Norwegian)

      -- 1. There are five houses.
      [c1, c2, c3, c4, c5] <- permutations [Red, Green, Ivory, Blue, Yellow]

      -- 6. The green house is immediately to the right of the ivory house.
      guard
        ( c1 == Ivory && c2 == Green
            || c2 == Ivory && c3 == Green
            || c3 == Ivory && c4 == Green
            || c4 == Ivory && c5 == Green
        )

      -- 2. The Englishman lives in the red house.
      guard
        ( n1 == Englishman && c1 == Red
            || n2 == Englishman && c2 == Red
            || n3 == Englishman && c3 == Red
            || n4 == Englishman && c4 == Red
            || n5 == Englishman && c5 == Red
        )

      -- 15. The Norwegian lives next to the blue house.
      guard
        ( n1 == Norwegian && c2 == Blue
            || n2 == Norwegian && (c1 == Blue || c3 == Blue)
            || n3 == Norwegian && (c2 == Blue || c4 == Blue)
            || n4 == Norwegian && (c3 == Blue || c5 == Blue)
            || n5 == Norwegian && c4 == Blue
        )

      [a1, a2, a3, a4, a5] <- permutations [Dog, Fox, Horse, Zebra, Snails]

      -- 3. The Spaniard owns the dog.
      guard
        ( n1 == Spaniard && a1 == Dog
            || n2 == Spaniard && a2 == Dog
            || n3 == Spaniard && a3 == Dog
            || n4 == Spaniard && a4 == Dog
            || n5 == Spaniard && a5 == Dog
        )

      [d1, d2, d3, d4, d5] <- permutations [Tea, Coffee, Milk, OrangeJuice, Water]

      -- 9. Milk is drunk in the middle house.
      guard (d3 == Milk)

      -- 4. Coffee is drunk in the green house.
      guard
        ( d1 == Coffee && c1 == Green
            || d2 == Coffee && c2 == Green
            || d3 == Coffee && c3 == Green
            || d4 == Coffee && c4 == Green
            || d5 == Coffee && c5 == Green
        )

      -- 5. The Ukrainian drinks tea.
      guard
        ( n1 == Ukrainian && d1 == Tea
            || n2 == Ukrainian && d2 == Tea
            || n3 == Ukrainian && d3 == Tea
            || n4 == Ukrainian && d4 == Tea
            || n5 == Ukrainian && d5 == Tea
        )

      [s1, s2, s3, s4, s5] <- permutations [OldGold, Kools, Chesterfield, Parliament, LuckyStrike]

      -- 7. The Old Gold smoker owns snails.
      guard
        ( s1 == OldGold && a1 == Snails
            || s2 == OldGold && a2 == Snails
            || s3 == OldGold && a3 == Snails
            || s4 == OldGold && a4 == Snails
            || s5 == OldGold && a5 == Snails
        )

      -- 8. Kools are smoked in the yellow house.
      guard
        ( s1 == Kools && c1 == Yellow
            || s2 == Kools && c2 == Yellow
            || s3 == Kools && c3 == Yellow
            || s4 == Kools && c4 == Yellow
            || s5 == Kools && c5 == Yellow
        )

      -- 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
      guard
        ( s1 == Chesterfield && a2 == Fox
            || s2 == Chesterfield && (a1 == Fox || a3 == Fox)
            || s3 == Chesterfield && (a2 == Fox || a4 == Fox)
            || s4 == Chesterfield && (a3 == Fox || a5 == Fox)
            || s5 == Chesterfield && a4 == Fox
        )

      -- 12. Kools are smoked in the house next to the house where the horse is kept.
      guard
        ( s1 == Kools && a2 == Horse
            || s2 == Kools && (a1 == Horse || a3 == Horse)
            || s3 == Kools && (a2 == Horse || a4 == Horse)
            || s4 == Kools && (a3 == Horse || a5 == Horse)
            || s5 == Kools && a4 == Horse
        )

      -- 13. The Lucky Strike smoker drinks orange juice.
      guard
        ( d1 == OrangeJuice && s1 == LuckyStrike
            || d2 == OrangeJuice && s2 == LuckyStrike
            || d3 == OrangeJuice && s3 == LuckyStrike
            || d4 == OrangeJuice && s4 == LuckyStrike
            || d5 == OrangeJuice && s5 == LuckyStrike
        )

      -- 14. The Japanese smokes Parliaments.
      guard
        ( n1 == Japanese && s1 == Parliament
            || n2 == Japanese && s2 == Parliament
            || n3 == Japanese && s3 == Parliament
            || n4 == Japanese && s4 == Parliament
            || n5 == Japanese && s5 == Parliament
        )

      return $
        Solution
          ( House (c1, n1, a1, d1, s1),
            House (c2, n2, a2, d2, s2),
            House (c3, n3, a3, d3, s3),
            House (c4, n4, a4, d4, s4),
            House (c5, n5, a5, d5, s5)
          )
