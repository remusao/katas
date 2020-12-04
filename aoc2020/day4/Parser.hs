{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Safe #-}

module Parser
  ( Parser(..)
  , anyString
  , asciiLower
  , char
  , choice
  , digit
  , eof
  , get
  , int
  , look
  , munch
  , oneOf
  , parse
  , pfail
  , satisfy
  , sepBy1
  , space
  , string
  ) where

import Text.Read qualified as T
import Control.Monad qualified as M
import Control.Monad.Fail qualified as Fail
import Data.Char qualified as C
import Control.Applicative qualified as A
import Control.Applicative

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
    fail _ = pfail


pfail :: Parser a
pfail = Parser (const Nothing)


anyString :: Parser String
anyString = Parser $ \s -> Just (s, [])


-- | This parser succeeds for any character. Returns the parsed character.
get :: Parser Char
get = Parser $ \case
    [] -> Nothing
    (c:xs) -> Just (c, xs)


look :: Parser String
look = Parser $ \case
    [] -> Nothing
    s -> Just (s, s)


eof :: Parser ()
eof = Parser $ \case
  [] -> Just ((), [])
  _ -> Nothing


choice :: [Parser a] -> Parser a
choice  [] = pfail
choice [p] = p
choice (p:ps) = p <|> choice ps


-- | The parser @satisfy f@ succeeds for any character for which the
-- supplied function @f@ returns 'True'. Returns the character that is
-- actually parsed.

-- >  digit     = satisfy isDigit
-- >  oneOf cs  = satisfy (\c -> c `elem` cs)
satisfy :: (Char -> Bool) -> Parser Char
satisfy allowed = do
    c <- get
    M.guard (allowed c)
    return c


oneOf :: [Char] -> Parser Char
oneOf cs  = satisfy (`elem` cs)


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


asciiLower :: Parser Char
asciiLower = satisfy C.isAsciiLower


-- | Parses an Integer.
int :: Parser Int
int = do
  digits <- A.many digit
  case T.readMaybe digits of
    Just int -> return int
    _ -> pfail


-- | Parses a space.
space :: Parser Char
space = satisfy C.isSpace


munch :: (Char -> Bool) -> Parser String
munch p = do
  s <- look
  scan s
  where
    scan (c:cs) | p c = do _ <- get; s <- scan cs; return (c:s)
    scan _            = do return ""


sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = M.liftM2 (:) p (A.many (sep >> p))


-- | Eval Parser 'p' against input String 's'.
parse :: Parser a -> String -> Maybe a
parse p s =
  case runParser p s of
    Nothing -> Nothing
    Just (a, _) -> Just a
