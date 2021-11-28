module Ribosome.Menu.Data.MenuRenderer where

import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuRenderEvent (MenuRenderEvent)

newtype MenuRenderer m i =
  MenuRenderer { unMenuRenderer :: MenuRenderEvent -> ReaderT (Menu i) m () }
