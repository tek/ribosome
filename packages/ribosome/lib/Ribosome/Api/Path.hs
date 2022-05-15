module Ribosome.Api.Path where

import Path (Abs, Dir, Path)

import Ribosome.Host.Api.Effect (vimCallFunction)
import Ribosome.Host.Effect.Rpc (Rpc)

nvimCwd ::
  Member Rpc r =>
  Sem r (Path Abs Dir)
nvimCwd =
  vimCallFunction "getcwd" []
