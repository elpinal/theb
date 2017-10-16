module Lib where

import Text.Parsec
import Text.Parsec.String

data HTML =
    Text String
  | Node String [HTML]

parseDoctype :: Parser String
parseDoctype = string "<!DOCTYPE html>"

parseTag :: Parser String
parseTag = do
  char '<'
  s <- many $ noneOf ">"
  char '>'
  return s
