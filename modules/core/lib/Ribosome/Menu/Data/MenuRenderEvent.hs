module Ribosome.Menu.Data.MenuRenderEvent where

import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuEvent (QuitReason)

data MenuRenderEvent m a =
  Render Bool Menu
  |
  Quit (QuitReason m a)
  deriving Show
