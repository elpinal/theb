module Lib where

import Text.Parsec
import Text.Parsec.String

data HTML =
    Elem String
  | Children [HTML]

parseTag :: Parser String
parseTag = do
  char '<'
  s <- many $ noneOf ">"
  char '>'
  return s
