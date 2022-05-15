module Ribosome.Api.Normal where

import Ribosome.Host.Api.Effect (nvimCommand)

normalm ::
  Member Rpc r =>
  Text ->
  m ()
normalm cmd =
  nvimCommand $ "normal " <> cmd

normal ::
  Member Rpc r =>
  Text ->
  m ()
normal cmd =
  nvimCommand $ "normal! " <> cmd

noautocmdNormal ::
  Member Rpc r =>
  Text ->
  m ()
noautocmdNormal cmd =
  nvimCommand $ "noautocmd normal! " <> cmd
