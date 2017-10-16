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

parseElement :: Parser HTML
parseElement =
  try parseElement' <|> do
    t <- parseTag
    xs <- manyTill parseElement' $ parseCloseTag
    return $ Node t xs

parseElement' :: Parser HTML
parseElement' = do
  t <- parseTag
  if t `elem` emptyElements
    then return $ Node t []
    else do
      s <- parseText
      u <- parseCloseTag
      if t == u
        then return $ Node t [Text s]
        else error "wrong close tag"

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
