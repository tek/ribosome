module Ribosome.Api.Undo where

import Ribosome.Host.Api.Effect (vimCommand)

undo ::
  Member Rpc r =>
  m ()
undo =
  nvimCommand "undo"
