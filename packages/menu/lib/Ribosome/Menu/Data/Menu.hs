module Ribosome.Menu.Data.Menu where

import Data.Trie (Trie)

import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.Entry (Entries)
import Ribosome.Menu.Data.MenuData (
  MenuCursor (MenuCursor),
  MenuItems (MenuItems),
  MenuQuery,
  )
import Ribosome.Menu.Data.MenuItem (Items)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data Menu i =
  Menu {
    items :: MenuItems i,
    cursor :: MenuCursor,
    prompt :: Prompt
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

consMenu :: Items i -> Entries i -> Trie (Entries i) -> Int -> MenuQuery -> CursorIndex -> Prompt -> Menu i
consMenu it en hist cnt curr curs =
  Menu (MenuItems it en hist cnt curr) (MenuCursor curs)
