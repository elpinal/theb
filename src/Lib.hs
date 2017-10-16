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
  parseElement
--  t <- parseTag
--  s <- parseText
--  case s of
--    "" -> do
--      u <- parseTag
--      s <- parseText
--      case s of
--        "" -> do
--          v <- parseTag
--          s <- parseText
--          return $ Node t [Node u [], Node v []]
--        _ -> return $ Node t [Node u [Text s]]
--    _ -> return $ Node t [Text s]

parseDoctype :: Parser String
parseDoctype = string "<!DOCTYPE html>"

parseElement :: Parser HTML
parseElement =
  try parseElement' <|> do
    t <- parseTag
    xs <- manyTill parseElement' $ try parseCloseTag
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
