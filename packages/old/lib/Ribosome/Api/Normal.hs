module Ribosome.Api.Normal where

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.IO (nvimCommand)

normalm ::
  NvimE e m =>
  Text ->
  m ()
normalm cmd =
  nvimCommand $ "normal " <> cmd

normal ::
  NvimE e m =>
  Text ->
  m ()
normal cmd =
  nvimCommand $ "normal! " <> cmd

noautocmdNormal ::
  NvimE e m =>
  Text ->
  m ()
noautocmdNormal cmd =
  nvimCommand $ "noautocmd normal! " <> cmd
