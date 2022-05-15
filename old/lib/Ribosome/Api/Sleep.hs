module Ribosome.Api.Sleep where

import Ribosome.Host.Api.Effect (nvimCommand)

nvimSleep ::
  Member Rpc r =>
  Int ->
  m ()
nvimSleep interval =
  nvimCommand $ "sleep " <> show interval

nvimMSleep ::
  Member Rpc r =>
  Int ->
  m ()
nvimMSleep interval =
  nvimCommand $ "sleep " <> show interval <> "m"
