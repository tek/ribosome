module Ribosome.Menu.Data.Menu where

import Data.DeepLenses (DeepLenses (..))
import Data.Trie (Trie)
import Prelude hiding (state)

import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.Entry (Entries)
import Ribosome.Menu.Data.MenuData (
  HasMenuCursor (menuCursor),
  HasMenuItems (menuItems),
  MenuCursor (MenuCursor),
  MenuItems (MenuItems),
  MenuQuery,
  )
import Ribosome.Menu.Data.MenuItem (Items)
import qualified Ribosome.Menu.Prompt.Data.Prompt as Prompt
import Ribosome.Menu.Prompt.Data.Prompt (HasPrompt, Prompt)

data Menu i =
  Menu {
    _items :: MenuItems i,
    _cursor :: MenuCursor,
    _prompt :: Prompt
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

makeClassy ''Menu

instance DeepLenses (Menu i) (Menu i) where
  deepLens = id

instance HasMenuItems (Menu i) i where
  menuItems =
    items

instance HasMenuCursor (Menu i) where
  menuCursor =
    cursor

instance HasPrompt (Menu i) where
  prompt =
    prompt

consMenu :: Items i -> Entries i -> Trie (Entries i) -> Int -> MenuQuery -> Bool -> CursorIndex -> Prompt -> Menu i
consMenu it en hist cnt curr dir curs =
  Menu (MenuItems it en hist cnt curr dir) (MenuCursor curs)
