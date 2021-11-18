module Ribosome.Menu.Data.Menu where

import Data.DeepLenses (DeepLenses (..))

import Ribosome.Menu.Data.FilteredMenuItem (FilteredMenuItem)
import Ribosome.Menu.Data.MenuItem (MenuItem)

newtype MenuFilter =
  MenuFilter Text
  deriving (Eq, Show)

instance Default MenuFilter where
  def = MenuFilter ""

data Menu a =
  Menu {
    _items :: [MenuItem a],
    _filtered :: [FilteredMenuItem a],
    _selected :: Int,
    _marked :: [Int],
    _currentQuery :: MenuFilter,
    _maxItems :: Maybe Int
  }
  deriving (Eq, Show, Generic, Default)

makeClassy ''Menu

instance DeepLenses (Menu a) (Menu a) where
  deepLens = id
