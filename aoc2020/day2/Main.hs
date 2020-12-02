{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import qualified Control.Monad as M
import qualified Control.Monad.Fail as Fail
import qualified Data.Char as C
import qualified Control.Applicative as A
import Data.Maybe (mapMaybe)

--
-- Custom Parser Combinator
--

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    -- 1. Run parser on input string.
    -- 2. Apply function on result of parsing.
    fmap f p = Parser $ \s ->
      case runParser p s of
        Nothing -> Nothing
        Just (a, rest) -> Just (f a, rest)


instance Applicative Parser where
    -- pure :: a -> Parser a
    -- Wrap a value inside a parser, leaving input unchanged.
    pure a = Parser $ \s -> Just (a, s)

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    -- 1. Run first parser on input (resulting in a function (a -> b).
    -- 2. Run second parser on remaining input, left by first parser.
    -- 3. Apply function (a -> b) on result of second parser.
    p1 <*> p2 = Parser $ \s ->
      case runParser p1 s of
        Nothing -> Nothing
        Just (f, rest) -> case runParser p2 rest of
          Nothing -> Nothing
          Just (a, rest2) -> Just (f a, rest2)


instance A.Alternative Parser where
    empty = Parser (const Nothing)
    p1 <|> p2 = Parser $ \s ->
        case runParser p1 s of
          Nothing -> runParser p2 s
          Just (a, rest) -> Just (a, rest)


instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    -- 1. Run first parser on input.
    -- 2. Feed result of parsing to `f`.
    -- 3. Run second parser (result of `f`) on remaining
    --    input (left by first parser)
    p >>= f = Parser $ \s ->
        case runParser p s of
            Nothing -> Nothing
            Just (a, rest) -> runParser (f a) rest

    -- return :: a -> Parser a
    return = pure


instance Fail.MonadFail Parser where
    -- fail :: String -> Parser a
    fail _ = Parser (const Nothing)


anyString :: Parser String
anyString = Parser $ \s -> Just (s, [])


-- | This parser succeeds for any character. Returns the parsed character.
anyChar :: Parser Char
anyChar = Parser $ \case
    [] -> Nothing
    (c:xs) -> Just (c, xs)


-- | The parser @satisfy f@ succeeds for any character for which the
-- supplied function @f@ returns 'True'. Returns the character that is
-- actually parsed.

-- >  digit     = satisfy isDigit
-- >  oneOf cs  = satisfy (\c -> c `elem` cs)
satisfy :: (Char -> Bool) -> Parser Char
satisfy allowed = do
    c <- anyChar
    M.guard (allowed c)
    return c


-- | @char c@ parses a single character @c@. Returns the parsed
-- character (i.e. @c@).
--
-- >  semiColon  = char ';'
char :: Char -> Parser Char
char c = satisfy (==c)


-- | @string s@ parses a sequence of characters given by @s@. Returns
-- the parsed string (i.e. @s@).
--
-- >  divOrMod    =   string "div"
-- >              <|> string "mod"
string :: String -> Parser String
string "" = return ""
string (c:cs) = do
    _ <- char c
    _ <- string cs
    return (c:cs)


-- | Parses an ASCII digit. Returns the parsed character.
digit :: Parser Char
digit = satisfy C.isDigit


-- | Parses an Integer.
int :: Parser Int
int = read <$> A.many digit


-- | Eval Parser 'p' against input String 's'.
parse :: Parser a -> String -> Maybe a
parse p s =
  case runParser p s of
    Nothing -> Nothing
    Just (a, _) -> Just a


--
-- Avent Of Code
--


data Password = Password
  { low :: Int
  , up :: Int
  , chr :: Char
  , password :: String
  }


parseInput :: String -> [Password]
parseInput = mapMaybe (parse password) . lines
    where
        password :: Parser Password
        password =
            Password <$>
              int
              <*> (M.void (char '-') *> int)
              <*> (M.void (char ' ') *> anyChar)
              <*> (M.void (string ": ") *> anyString)


solve1 :: [Password] -> Int
solve1 = length . filter isValid
  where
    isValid :: Password -> Bool
    isValid Password { low, up, chr, password } =
      let count = length . filter (== chr) $ password
       in count >= low && count <= up


solve2 :: [Password] -> Int
solve2 = length . filter isValid
  where
    isValid :: Password -> Bool
    isValid Password { low, up, chr, password } = go password 1 False
      where
        go :: String -> Int -> Bool -> Bool
        go [] _ acc = acc
        go (c:cs) idx acc =
          go cs (idx + 1) (
            if idx == low || idx == up
               then acc /= (c == chr)
               else acc
          )


main :: IO ()
main = interact $ show . solve2 . parseInput
