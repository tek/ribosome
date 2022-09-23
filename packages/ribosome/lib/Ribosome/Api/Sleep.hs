-- |API functions for sleeping in Neovim.
module Ribosome.Api.Sleep where

import Exon (exon)

import Ribosome.Host.Api.Data (nvimCommand)
import Ribosome.Host.Effect.Rpc (Rpc)

-- |Run the @sleep@ command.
nvimSleep ::
  Member Rpc r =>
  Int ->
  Sem r ()
nvimSleep interval =
  nvimCommand [exon|sleep #{show interval}|]

-- |Run the @sleep@ command with the number interpreted as milliseconds.
nvimMSleep ::
  Member Rpc r =>
  Int ->
  Sem r ()
nvimMSleep interval =
  nvimCommand [exon|sleep #{show interval}m|]
