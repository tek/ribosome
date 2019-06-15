module Ribosome.Api.Autocmd where

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.IO (vimCommand)

doautocmd ::
  NvimE e m =>
  Text ->
  m ()
doautocmd name =
  vimCommand $ "doautocmd " <> name

uautocmd ::
  NvimE e m =>
  Text ->
  m ()
uautocmd name =
  doautocmd $ "User " <> name
