module Browser.Theb where

class Http h where
  get :: String -> h String
