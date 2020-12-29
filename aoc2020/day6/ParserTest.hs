
module Main (main) where

import Data.Functor
import Control.Monad
import Control.Applicative hiding (many)
import Data.Maybe

import Parser

test :: Eq a => String -> Parser a -> String -> Maybe a -> IO ()
test name p input expected = do
  let got = parse p input
  putStrLn (show (got == expected) ++ " > " ++ name)

main :: IO ()
main = do
  -- pfail
  test "pfail with empty string" (pfail :: Parser Int) "" Nothing
  test "pfail with non-empty string" (pfail :: Parser Int) "foo" Nothing
  let Just (r, rest) = runParser ((pfail :: Parser ()) <|> return ()) "foo"
  print (r, rest)
  putStrLn ""

  -- anyString
  test "anyString with empty string" anyString "" (Just "")
  test "anyString with non-empty string" anyString "foo" (Just "foo")
  let Just (r, rest) = runParser anyString "foo"
  print (r, rest)
  putStrLn ""

  -- get
  test "get with empty string" get "" Nothing
  test "get with non-empty string" get "f" (Just 'f')
  test "get with non-empty string" get "foo" (Just 'f')
  let Just (r, rest) = runParser get "foo"
  print (r, rest)
  putStrLn ""

  -- eof
  test "eof with empty string" eof "" (Just ())
  test "eof with non-empty string" eof "foo" Nothing
  putStrLn ""

  -- char
  test "char with empty string" (char 'f') "" Nothing
  test "char with non-empty string" (char 'f') "bar" Nothing
  test "char with non-empty string" (char 'f') "foo" (Just 'f')
  putStrLn ""

  -- satisfy
  test "satisfy with empty string" (satisfy (== 'f')) "" Nothing
  test "satisfy with non-empty string" (satisfy (== 'f')) "f" (Just 'f')
  test "satisfy with non-empty string" (satisfy (== 'f')) "foo" (Just 'f')
  let Just (r, rest) = runParser (satisfy (== 'b') <|> pure 'f') "foo"
  print (r, rest)
  putStrLn ""

  -- oneOf
  test "oneOf with empty string" (oneOf ['f', 'b']) "" Nothing
  test "oneOf with non-empty string" (oneOf ['f', 'b']) "foo" (Just 'f')
  test "oneOf with non-empty string" (oneOf ['f', 'b']) "bar" (Just 'b')
  let Just (r, rest) = runParser (oneOf ['f', 'b'] <|> pure 'z') "?"
  print (r, rest)
  putStrLn ""

  -- string
  test "string with empty string" (string "foo") "" Nothing
  test "string with non-empty string" (string "") "" (Just "")
  test "string with non-empty string" (string "foo") "foo" (Just "foo")
  test "string with non-empty string" (string "foo") "foobar" (Just "foo")
  test "string with non-empty string" (string "foo") "bar" Nothing
  let Just (r, rest) = runParser (string "foo") "foo bar"
  print (r, rest)
  putStrLn ""

  -- choice
  test "choice with empty string" (choice [void get, eof]) "" (Just ())
  test "choice with non-empty string" (choice [get, eof $> 'b']) "foo" (Just 'f')
  let Just (r, rest) = runParser (choice [char 'f', pure 'b']) "bar"
  print (r, rest)
  putStrLn ""

  -- int
  test "int with empty string" int "" Nothing
  test "int with number" int "42" (Just 42)
  test "int with number + rest" int "42foo" (Just 42)
  test "int with no number" int "foo" Nothing
  putStrLn ""

  -- sepBy1
  test "sepBy1 with empty string" (get `sepBy1` get) "" Nothing
  test "sepBy1 with empty string" (get `sepBy1` get) "a" (Just ['a'])
  test "sepBy1 with empty string" (get `sepBy1` get) "ab" (Just ['a'])
  test "sepBy1 with empty string" (get `sepBy1` get) "abc" (Just ['a', 'c'])
  test "sepBy1 with empty string" (get `sepBy1` string "b") "abc" (Just ['a', 'c'])
  test "sepBy1 with empty string" (get `sepBy1` string "\n") "a\nc" (Just ['a', 'c'])
  test "sepBy1 with empty string" (char 'a' `sepBy1` string "\n") "a\nc" (Just ['a'])
  let Just (r, rest) = runParser (many (char 'a') `sepBy1` string "n") "aaaaanc"
  print (r, rest)
