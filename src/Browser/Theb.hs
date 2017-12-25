module Browser.Theb where

type URI = String

class Http h where
  get :: URI -> h String
