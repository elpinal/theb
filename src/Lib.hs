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
--  t <- parseStartTag
--  s <- parseText
--  case s of
--    "" -> do
--      u <- parseStartTag
--      s <- parseText
--      case s of
--        "" -> do
--          v <- parseStartTag
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
    t <- parseStartTag
    xs <- manyTill parseElement $ try parseEndTag
    return $ Node t xs

parseElement' :: Parser HTML
parseElement' = do
  t <- parseStartTag
  if t `elem` voidElements
    then return $ Node t []
    else do
      s <- parseTextMay
      u <- parseEndTag
      if t == u
        then return . Node t $ maybeToList s
        else error "wrong close tag"

parseTextMay :: Parser (Maybe HTML)
parseTextMay = do
  s <- manyTill anyChar . lookAhead $ string "<"
  if s /= ""
    then return . Just $ Text s
    else return Nothing

parseStartTag :: Parser String
parseStartTag = do
  char '<'
  manyTill alphaNum $ char '>'

parseEndTag :: Parser String
parseEndTag = do
  string "</"
  manyTill alphaNum $ many whitespace >> char '>'

whitespace :: Parser Char
whitespace = (choice . map char)
  [ '\x09'
  , '\x0a'
  , '\x0c'
  , '\x0d'
  , ' '
  ] <?> "whitespace"

voidElements :: [String]
voidElements = ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]
