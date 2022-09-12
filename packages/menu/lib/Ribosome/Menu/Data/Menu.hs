module Ribosome.Menu.Data.Menu where

import Data.Trie (Trie)

import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.Entry (Entries)
import Ribosome.Menu.Data.MenuItem (Items)
import Ribosome.Menu.Data.MenuItems (MenuItems (MenuItems), MenuQuery)

data Menu filter i =
  Menu {
    items :: MenuItems filter i,
    cursor :: CursorIndex
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

consMenu ::
  Items i ->
  Entries i ->
  Map filter (Trie (Entries i)) ->
  Int ->
  Int ->
  MenuQuery ->
  filter ->
  CursorIndex ->
  Menu filter i
consMenu it en hist cnt ecnt curr currF curs =
  Menu (MenuItems it en hist cnt ecnt curr currF) curs
