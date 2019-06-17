module Ribosome.Api.Autocmd where

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.IO (vimCommand)

doautocmd ::
  NvimE e m =>
  Bool ->
  Text ->
  m ()
doautocmd silent name =
  vimCommand $ pre <> "doautocmd " <> name
  where
    pre =
      if silent then "silent " else ""

uautocmd ::
  NvimE e m =>
  Bool ->
  Text ->
  m ()
uautocmd silent name =
  doautocmd silent $ "User " <> name
