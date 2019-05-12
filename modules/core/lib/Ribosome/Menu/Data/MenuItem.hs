module Ribosome.Menu.Data.MenuItem where

data MenuItem =
  MenuItem {
    ident :: Text,
    text :: Text
  }
  deriving (Eq, Show)
