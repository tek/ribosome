module Ribosome.Menu.Data.MenuRenderEvent where

import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuEvent (QuitReason)

data MenuRenderEvent m a i =
  Render Bool (Menu i)
  |
  Quit (QuitReason m a)
  deriving Show
