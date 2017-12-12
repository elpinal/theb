module Lib where

import Text.Parsec
import Text.Parsec.String

import Data.Maybe

data HTML =
    Text String
  | Node String [Attr] [HTML]
  deriving (Eq, Show)

type Attr = (String, String)

parse' :: Parser a -> String -> Either ParseError a
parse' p s = parse p "" s

parseHTML :: Parser HTML
parseHTML = parseDoctype >> parseElement

doctype :: String
doctype = "<!DOCTYPE html>"

parseDoctype :: Parser String
parseDoctype = string doctype

parseElement :: Parser HTML
parseElement =
  try parseElement' <|> do
    (t, _) <- parseStartTag
    xs <- manyTill parseElement $ try parseEndTag
    return $ Node t [] xs

parseElement' :: Parser HTML
parseElement' = do
  (t, _) <- parseStartTag
  if t `elem` voidElements
    then return $ Node t [] []
    else do
      s <- parseTextMay
      u <- parseEndTag
      if t == u
        then return . Node t [] $ maybeToList s
        else error "wrong close tag"

parseTextMay :: Parser (Maybe HTML)
parseTextMay = do
  s <- manyTill anyChar . lookAhead $ string "<"
  if s /= ""
    then return . Just $ Text s
    else return Nothing

parseStartTag :: Parser (String, [Attr])
parseStartTag = do
  char '<'
  s <- many1 alphaNum
  as <- many $ try $ skipMany whitespace >> parseAttr
  char '>'
  return (s, as)

-- TODO: The use of alphaNum is too conservative.
parseAttr :: Parser Attr
parseAttr = do
  k <- many1 alphaNum
  skipMany whitespace
  v <- option "" $ char '=' >> skipMany whitespace >> parseValue
  return (k, v)

parseValue :: Parser String
parseValue = choice
  [ many1 alphaNum
  , quoted $ many $ noneOf "'"
  , doubleQuoted $ many $ noneOf "\""
  ]

quoted :: Parser a -> Parser a
quoted = between (char '\'') (char '\'')

doubleQuoted :: Parser a -> Parser a
doubleQuoted = between (char '"') (char '"')

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
