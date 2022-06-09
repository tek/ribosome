module Ribosome.Menu.Settings where

import Ribosome.Data.Setting (Setting (Setting))

menuCloseFloats :: Setting Bool
menuCloseFloats =
  Setting "ribosome_menu_close_floats" False (Just True)
