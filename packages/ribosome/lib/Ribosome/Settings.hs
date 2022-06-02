module Ribosome.Settings (
  module Ribosome.Data.Setting,
  module Ribosome.Settings,
  module Ribosome.Effect.Settings,
) where

import Ribosome.Data.Setting (Setting (..))
import Ribosome.Effect.Settings (Settings, get, maybe, or, update)

tmuxSocket :: Setting FilePath
tmuxSocket =
  Setting "tmux_socket" True Nothing

menuCloseFloats :: Setting Bool
menuCloseFloats =
  Setting "ribosome_menu_close_floats" False (Just True)
