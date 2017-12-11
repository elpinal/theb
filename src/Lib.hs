module Lib where

import Text.Parsec
import Text.Parsec.String

import Data.Maybe

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

doctype :: String
doctype = "<!DOCTYPE html>"

parseDoctype :: Parser String
parseDoctype = string doctype

parseElement :: Parser HTML
parseElement =
  try parseElement' <|> do
    t <- parseTag
    xs <- manyTill parseElement $ try parseCloseTag
    return $ Node t xs

parseElement' :: Parser HTML
parseElement' = do
  t <- parseTag
  if t `elem` emptyElements
    then return $ Node t []
    else do
      s <- parseTextMay
      u <- parseCloseTag
      if t == u
        then return . Node t $ maybeToList s
        else error "wrong close tag"

parseTextMay :: Parser (Maybe HTML)
parseTextMay = do
  s <- manyTill anyChar . lookAhead $ string "<"
  if s /= ""
    then return . Just $ Text s
    else return Nothing

parseTag :: Parser String
parseTag = do
  char '<'
  manyTill alphaNum $ char '>'

parseCloseTag :: Parser String
parseCloseTag = do
  string "</"
  s <- many $ noneOf ">"
  char '>'
  return s

emptyElements :: [String]
emptyElements = ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]
