module Lib where

import Text.Parsec
import Text.Parsec.String

data HTML =
    Text String
  | Node String [HTML]
  deriving (Eq, Show)

parse' :: Parser a -> String -> Either ParseError a
parse' p s = parse p "" s

parseHTML :: Parser HTML
parseHTML = do
  parseDoctype
  t <- parseTag
  i <- manyTill anyChar (try (string "<"))
  return $ Node t [Text i]

parseDoctype :: Parser String
parseDoctype = string "<!DOCTYPE html>"

parseText :: Parser String
parseText = manyTill anyChar (lookAhead (string "<"))

parseTag :: Parser String
parseTag = do
  char '<'
  s <- many $ noneOf ">"
  char '>'
  return s

parseCloseTag :: Parser String
parseCloseTag = do
  string "</"
  s <- many $ noneOf ">"
  char '>'
  return s

emptyElements :: [String]
emptyElements = ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]
