-- |API functions for process IDs.
module Ribosome.Api.Process where

import Ribosome.Host.Api.Effect (vimCallFunction)
import Ribosome.Host.Effect.Rpc (Rpc)

-- |Return Neovim's process ID.
vimPid ::
  Member Rpc r =>
  Sem r Int
vimPid =
  vimCallFunction "getpid" []
