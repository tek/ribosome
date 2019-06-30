module Ribosome.Menu.Data.MenuItem where

data MenuItem a =
  MenuItem {
    _meta :: a,
    _text :: Text
  }
  deriving (Eq, Show, Ord)

makeClassy ''MenuItem
