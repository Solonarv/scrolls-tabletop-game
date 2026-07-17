module Scrolls.Card where

import Scrolls.Engine qualified as Engine
import Data.Text (Text)

data Prototype = Prototype
  { name :: Text
  , cost :: Int
  , body :: Engine.Action Int
  }

