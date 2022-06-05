module Ribosome.Host.Error where

import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)

ignoreRpcError ::
  Member (Rpc !! RpcError) r =>
  Sem (Rpc : r) a ->
  Sem r ()
ignoreRpcError =
  resume_ . void

resumeBootError ::
  ∀ eff err r .
  Show err =>
  Members [eff !! err, Error BootError] r =>
  InterpreterFor eff r
resumeBootError =
  resumeHoistError @_ @eff (BootError . show @Text)
