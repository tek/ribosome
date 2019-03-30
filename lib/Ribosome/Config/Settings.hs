module Ribosome.Config.Settings where

import Ribosome.Data.Setting (Setting(Setting))

persistenceDir :: Setting FilePath
persistenceDir = Setting "ribosome_persistence_dir" False Nothing

tmuxSocket :: Setting FilePath
tmuxSocket = Setting "tmux_socket" True Nothing
