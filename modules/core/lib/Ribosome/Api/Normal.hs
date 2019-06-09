module Ribosome.Api.Normal where

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.IO (vimCommand)

normalm ::
  NvimE e m =>
  Text ->
  m ()
normalm cmd =
  vimCommand $ "normal " <> cmd

normal ::
  NvimE e m =>
  Text ->
  m ()
normal cmd =
  vimCommand $ "normal! " <> cmd
