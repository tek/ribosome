module Ribosome.Api.Window where

import Control.Monad (when)

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.Data (Window)
import Ribosome.Nvim.Api.IO (nvimWinClose, windowIsValid)

closeWindow ::
  NvimE e m =>
  Window ->
  m ()
closeWindow window = do
  valid <- windowIsValid window
  when valid $ nvimWinClose window True
