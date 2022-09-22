module Ribosome.Menu.Data.FilterMode where

import Ribosome.Menu.Data.MenuItem (MenuItem)

data FilterMode f i =
  FilterMode {
    filter :: f,
    extract :: MenuItem i -> Maybe Text
  }
