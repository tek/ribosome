module Ribosome.Api.Sleep where

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.IO (vimCommand)

nvimSleep ::
  NvimE e m =>
  Int ->
  m ()
nvimSleep interval =
  vimCommand $ "sleep " ++ show interval

nvimMSleep ::
  NvimE e m =>
  Int ->
  m ()
nvimMSleep interval =
  vimCommand $ "sleep " ++ show interval ++ "m"
