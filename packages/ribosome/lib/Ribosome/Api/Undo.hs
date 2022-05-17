module Ribosome.Api.Undo where

import Ribosome.Host.Api.Effect (nvimCommand)
import Ribosome.Host.Effect.Rpc (Rpc)

undo ::
  Member Rpc r =>
  Sem r ()
undo =
  nvimCommand "undo"
