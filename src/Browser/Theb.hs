module Browser.Theb where

import Lib

type URI = String

class Http h where
  get :: URI -> h String

open :: Http h => h HTML
