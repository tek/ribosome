module Ribosome.Menu.Data.NvimMenuConfig where

import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Menu.Data.MenuConfig (MenuConfig)

data NvimMenuConfig i =
  NvimMenuConfig {
    menu :: MenuConfig i,
    scratch :: ScratchOptions
  }
  deriving stock (Generic)
