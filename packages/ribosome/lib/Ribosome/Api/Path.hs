module Ribosome.Api.Path where

import Path (Abs, Dir, Path)

import Ribosome.Host.Api.Effect (vimCallFunction, nvimCommand)
import Ribosome.Host.Effect.Rpc (Rpc)
import Exon (exon)
import Ribosome.Path (pathText)

nvimCwd ::
  Member Rpc r =>
  Sem r (Path Abs Dir)
nvimCwd =
  vimCallFunction "getcwd" []

nvimSetCwd ::
  Member Rpc r =>
  Path Abs Dir ->
  Sem r ()
nvimSetCwd dir =
  nvimCommand [exon|cd #{pathText dir}|]
