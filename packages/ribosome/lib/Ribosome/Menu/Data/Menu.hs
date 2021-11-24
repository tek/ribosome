module Ribosome.Menu.Data.Menu where

import Control.Lens (Optic', to)
import Data.DeepLenses (DeepLenses (..))

import Ribosome.Menu.Data.FilteredMenuItem (FilteredMenuItem)
import Ribosome.Menu.Data.MenuItem (MenuItem)

newtype MenuFilter =
  MenuFilter Text
  deriving stock (Eq, Show)

instance Default MenuFilter where
  def = MenuFilter ""

data Menu a =
  Menu {
    _items :: [MenuItem a],
    _filtered :: Maybe [FilteredMenuItem a],
    _history :: [[FilteredMenuItem a]],
    _selected :: Int,
    _marked :: [Int],
    _currentQuery :: MenuFilter,
    _maxItems :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

makeClassy ''Menu

instance DeepLenses (Menu a) (Menu a) where
  deepLens = id

push ::
  [FilteredMenuItem a] ->
  Menu a ->
  Menu a
push new m =
  m { _filtered = Just new, _history = foldMap pure (_filtered m) ++ _history m }

current :: Contravariant f => Optic' (->) f (Menu a) [FilteredMenuItem a]
current =
  to (fold . _filtered)

numVisible :: Menu a -> Int
numVisible =
  maybe 0 length . _filtered
