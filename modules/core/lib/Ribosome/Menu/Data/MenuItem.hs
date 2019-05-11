module Ribosome.Menu.Data.MenuItem where

newtype MenuItem =
  MenuItem {
    text :: Text
  }
  deriving (Eq, Show)
