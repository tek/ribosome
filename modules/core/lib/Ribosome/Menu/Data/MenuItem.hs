module Ribosome.Menu.Data.MenuItem where

data MenuItem a =
  MenuItem {
    _meta :: a,
    _text :: Text,
    _abbreviated :: Text
  }
  deriving (Eq, Show, Ord)

makeClassy ''MenuItem

simpleMenuItem :: a -> Text -> MenuItem a
simpleMenuItem a t =
  MenuItem a t t
