-- |Functions for triggering normal mode commands.
module Ribosome.Api.Normal where

import Ribosome.Host.Api.Data (nvimCommand)

import Ribosome.Host.Effect.Rpc (Rpc)
import Exon (exon)

-- |Execute a sequence of characters in normal mode that may trigger mappings.
normalm ::
  Member Rpc r =>
  Text ->
  Sem r ()
normalm cmd =
  nvimCommand [exon|normal #{cmd}|]

-- |Execute a sequence of characters in normal mode that may not trigger mappings.
normal ::
  Member Rpc r =>
  Text ->
  Sem r ()
normal cmd =
  nvimCommand [exon|normal! #{cmd}|]
