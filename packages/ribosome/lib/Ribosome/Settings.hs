module Ribosome.Settings (
  module Ribosome.Data.Setting,
  module Ribosome.Data.SettingError,
  module Ribosome.Settings,
  module Ribosome.Effect.Settings,
) where

import Ribosome.Data.Setting (Setting (..))
import Ribosome.Data.SettingError (SettingError (..))
import Ribosome.Effect.Settings (Settings, get, maybe, or, update)

tmuxSocket :: Setting FilePath
tmuxSocket =
  Setting "tmux_socket" True Nothing
