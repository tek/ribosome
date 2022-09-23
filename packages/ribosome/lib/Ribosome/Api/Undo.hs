-- |API functions for @:undo@.
module Ribosome.Api.Undo where

import Ribosome.Host.Api.Data (nvimCommand)
import Ribosome.Host.Effect.Rpc (Rpc)

-- |Run @:undo@.
undo ::
  Member Rpc r =>
  Sem r ()
undo =
  nvimCommand "undo"
