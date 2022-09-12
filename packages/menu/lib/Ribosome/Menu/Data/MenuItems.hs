module Ribosome.Menu.Data.MenuItems where

import Data.Trie (Trie)

import Ribosome.Menu.Data.Entry (Entries)
import Ribosome.Menu.Data.MenuItem (Items)
import qualified Data.Map.Strict as Map

newtype MenuQuery =
  MenuQuery Text
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, Ord, IsString)

instance Default MenuQuery where
  def = MenuQuery ""

data MenuItems filter a =
  MenuItems {
    items :: Items a,
    entries :: Entries a,
    history :: Map filter (Trie (Entries a)),
    itemCount :: Int,
    entryCount :: Int,
    currentQuery :: MenuQuery,
    currentFilter :: filter
  }
  deriving stock (Eq, Show, Generic)

instance (
    Default filter
  ) => Default (MenuItems filter a) where
  def =
    MenuItems mempty mempty Map.empty 0 0 mempty def

cons ::
  filter ->
  MenuItems filter a
cons =
  MenuItems mempty mempty Map.empty 0 0 mempty
