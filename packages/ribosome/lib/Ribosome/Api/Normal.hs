module Ribosome.Api.Normal where

import Ribosome.Host.Api.Effect (nvimCommand)

import Ribosome.Host.Effect.Rpc (Rpc)
import Exon (exon)

normalm ::
  Member Rpc r =>
  Text ->
  Sem r ()
normalm cmd =
  nvimCommand [exon|normal #{cmd}|]

normal ::
  Member Rpc r =>
  Text ->
  Sem r ()
normal cmd =
  nvimCommand [exon|normal! #{cmd}|]

noautocmdNormal ::
  Member Rpc r =>
  Text ->
  Sem r ()
noautocmdNormal cmd =
  nvimCommand [exon|noautocmd normal! #{cmd}|]
