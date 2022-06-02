module Ribosome.Menu.Data.MenuData where

import Control.Lens (makeClassy)
import Data.Trie (Trie)

import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.Entry (Entries)
import Ribosome.Menu.Data.MenuItem (Items)

newtype MenuQuery =
  MenuQuery Text
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, Ord, IsString)

instance Default MenuQuery where
  def = MenuQuery ""

data MenuItems a =
  MenuItems {
    _items :: Items a,
    _entries :: Entries a,
    _history :: Trie (Entries a),
    _itemCount :: Int,
    _currentQuery :: MenuQuery
  }
  deriving stock (Eq, Show)

makeClassy ''MenuItems

instance Default (MenuItems a) where
  def =
    MenuItems mempty mempty mempty 0 mempty

newtype MenuCursor =
  MenuCursor { _cursor :: CursorIndex }
  deriving stock (Eq, Show, Generic)

makeClassy ''MenuCursor

instance Default MenuCursor where
  def =
    MenuCursor 0
