module Ribosome.Menu.Data.MenuData where

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
    items :: Items a,
    entries :: Entries a,
    history :: Trie (Entries a),
    itemCount :: Int,
    currentQuery :: MenuQuery
  }
  deriving stock (Eq, Show, Generic)

instance Default (MenuItems a) where
  def =
    MenuItems mempty mempty mempty 0 mempty

newtype MenuCursor =
  MenuCursor { cursor :: CursorIndex }
  deriving stock (Eq, Show, Generic)

instance Default MenuCursor where
  def =
    MenuCursor 0
