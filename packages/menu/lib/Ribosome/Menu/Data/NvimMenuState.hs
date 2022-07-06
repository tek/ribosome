module Ribosome.Menu.Data.NvimMenuState where

import Data.Generics.Labels ()

import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.CursorLine (CursorLine)
import Ribosome.Menu.Data.MenuView (MenuView)

data NvimMenuState =
  NvimMenuState {
    view :: MenuView,
    cursorIndex :: CursorIndex,
    indexes :: [(Int, Bool)]
  }
  deriving stock (Eq, Show, Generic)

instance Default NvimMenuState where
  def =
    NvimMenuState def 0 def

topIndex :: Lens' NvimMenuState Int
topIndex =
  #view . #topIndex

botIndex :: Lens' NvimMenuState Int
botIndex =
  #view . #botIndex

cursor :: Lens' NvimMenuState CursorIndex
cursor =
  #view . #cursor

cursorLine :: Lens' NvimMenuState CursorLine
cursorLine =
  #view . #cursorLine
