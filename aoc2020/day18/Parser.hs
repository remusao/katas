{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Safe #-}

module Parser
  ( Parser(..)
  , anyString
  , asciiLower
  , chainl
  , chainl1
  , char
  , choice
  , count
  , digit
  , eof
  , get
  , int
  , look
  , many
  , many1
  , munch
  , oneOf
  , parse
  , pfail
  , satisfy
  , sepBy
  , sepBy1
  , space
  , string
  ) where

import Data.Functor (($>))
import Data.Maybe (maybe)
import Text.Read qualified as T
import Control.Monad qualified as M
import Control.Monad.Fail qualified as Fail
import Data.Char qualified as C
import Control.Applicative qualified as A
import Control.Applicative ((<|>))

--
-- Custom Parser Combinator
--

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }


instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    -- 1. Run parser on input string.
    -- 2. Apply function on result of parsing.
    fmap f (Parser p) = Parser $ \s ->
      case p s of
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
    Parser p1 <*> Parser p2 = Parser $ \s ->
      case p1 s of
        Nothing -> Nothing
        Just (f, rest) ->
          case p2 rest of
            Nothing -> Nothing
            Just (a, rest2) -> Just (f a, rest2)


instance A.Alternative Parser where
    empty = pfail
    Parser p1 <|> Parser p2 = Parser $ \s ->
        case p1 s of
          Nothing -> p2 s
          result -> result


instance Monad Parser where
    -- return :: a -> Parser a
    return = pure

    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    -- 1. Run first parser on input.
    -- 2. Feed result of parsing to `f`.
    -- 3. Run second parser (result of `f`) on remaining
    --    input (left by first parser)
    (Parser p) >>= f = Parser $ \s ->
        case p s of
            Nothing -> Nothing
            Just (a, rest) -> runParser (f a) rest


instance Fail.MonadFail Parser where
    -- fail :: String -> Parser a
    fail _ = pfail


-- | Parser which alway fails to parse its input.
pfail :: Parser a
pfail = Parser (const Nothing)


-- | Return whatever string remains.
anyString :: Parser String
anyString = Parser $ \s -> Just (s, [])


-- | This parser succeeds for any character. Returns the parsed character.
get :: Parser Char
get = Parser $ \case
    [] -> Nothing
    (c:xs) -> Just (c, xs)


-- | Returns remaining input without consuming it.
look :: Parser String
look = Parser $ \case
    [] -> Nothing
    s -> Just (s, s)


-- | Parse end of file. Will only succeed whenever all input is already consumed.
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
satisfy allowed = Parser $ \case
    (c:cs) | allowed c -> Just (c, cs)
    _ -> Nothing


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
string "" = pure ""
string this = look >>= scan this
  where
    scan (x:xs) (y:ys)
      | x == y = get *> scan xs ys
    scan "" _ = pure this
    scan _ _ = pfail


-- | Parses an ASCII digit. Returns the parsed character.
digit :: Parser Char
digit = satisfy C.isDigit


asciiLower :: Parser Char
asciiLower = satisfy C.isAsciiLower


-- | Parses an Integer.
sint :: Parser Int
sint = do
  sign <- choice [char '-' $> (-1), char '+' $> 1]
  many digit >>= maybe pfail (return . (* sign)) . T.readMaybe


-- | Parses an Integer.
int :: Parser Int
int = many digit >>= maybe pfail return . T.readMaybe


-- | Parses a space.
space :: Parser Char
space = satisfy C.isSpace


munch :: (Char -> Bool) -> Parser String
munch p = look >>= scan
  where
    scan (c:cs) | p c = (:) <$> get <*> scan cs
    scan _            = return []


-- | Parses zero or more occurrences of the given parser.
many :: Show a => Parser a -> Parser [a]
many p = many1 p <|> return []


-- | Parses one or more occurrences of the given parser.
many1 :: Show a => Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p


sepBy :: Show a => Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> return []


sepBy1 :: Show a => Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)


count :: Int -> Parser a -> Parser [a]
count = M.replicateM


chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
-- ^ @chainl p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /left/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainl p op x = chainl1 p op <|> return x


chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
-- ^ Like 'chainl', but parses one or more occurrences of @p@.
chainl1 p op = p >>= rest
  where rest x = do f <- op
                    y <- p
                    rest (f x y)
                 <|> return x


-- | Eval Parser 'p' against input String 's'.
parse :: Parser a -> String -> Maybe a
parse p s =
  case runParser p s of
    Nothing -> Nothing
    Just (a, _) -> Just a
