module Ribosome.Api.Tabpage where

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.Data (Tabpage)
import Ribosome.Nvim.Api.IO (nvimTabpageGetNumber, tabpageIsValid, vimCommand)

closeTabpage ::
  NvimE e m =>
  Tabpage ->
  m ()
closeTabpage tabpage = do
  valid <- tabpageIsValid tabpage
  when valid $ do
    number <- nvimTabpageGetNumber tabpage
    vimCommand ("silent! tabclose! " <> show number)
