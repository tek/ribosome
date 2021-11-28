module Ribosome.Menu.Data.NvimMenuState where

import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.MenuView (MenuView, HasMenuView (menuView))

data NvimMenuState =
  NvimMenuState {
    _view :: MenuView,
    _cursorIndex :: CursorIndex,
    _indexes :: [Int]
  }
  deriving stock (Eq, Show)

makeClassy ''NvimMenuState

instance Default NvimMenuState where
  def =
    NvimMenuState def 0 def

instance HasMenuView NvimMenuState where
  menuView =
    view
