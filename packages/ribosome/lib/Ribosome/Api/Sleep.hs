module Ribosome.Api.Sleep where

import Exon (exon)

import Ribosome.Host.Api.Effect (nvimCommand)
import Ribosome.Host.Effect.Rpc (Rpc)

nvimSleep ::
  Member Rpc r =>
  Int ->
  Sem r ()
nvimSleep interval =
  nvimCommand [exon|sleep #{show interval}|]

nvimMSleep ::
  Member Rpc r =>
  Int ->
  Sem r ()
nvimMSleep interval =
  nvimCommand [exon|sleep #{show interval}m|]
