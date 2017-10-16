module Lib where

data HTML =
    Elem String
  | Children [HTML]
