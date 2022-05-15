module Ribosome.Api.Sleep where

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.IO (nvimCommand)

nvimSleep ::
  NvimE e m =>
  Int ->
  m ()
nvimSleep interval =
  nvimCommand $ "sleep " <> show interval

nvimMSleep ::
  NvimE e m =>
  Int ->
  m ()
nvimMSleep interval =
  nvimCommand $ "sleep " <> show interval <> "m"
