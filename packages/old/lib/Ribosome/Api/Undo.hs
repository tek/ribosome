module Ribosome.Api.Undo where

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.IO (vimCommand)

undo ::
  NvimE e m =>
  m ()
undo =
  nvimCommand "undo"
