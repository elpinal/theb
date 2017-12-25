module Browser.Theb where

import Lib

type URI = String

class Monad h => Http h where
  get :: URI -> h String

open :: Http h => URI -> h (Either Error HTML)
