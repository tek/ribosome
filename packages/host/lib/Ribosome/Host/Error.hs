module Ribosome.Host.Error where

import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)

-- |Run a 'Sem' that uses 'Rpc' and discard 'RpcError's, interpreting 'Rpc' to @'Rpc' '!!' 'RpcError'@.
ignoreRpcError ::
  Member (Rpc !! RpcError) r =>
  Sem (Rpc : r) a ->
  Sem r ()
ignoreRpcError =
  resume_ . void

-- |Run a 'Sem' that uses 'Rpc' and catch 'RpcError's with the supplied function, interpreting 'Rpc' to @'Rpc' '!!'
-- 'RpcError'@.
onRpcError ::
  Member (Rpc !! RpcError) r =>
  (RpcError -> Sem r a) ->
  Sem (Rpc : r) a ->
  Sem r a
onRpcError =
  resuming

-- |Resume an error by transforming it to @'Error' 'BootError'@.
resumeBootError ::
  ∀ eff err r .
  Show err =>
  Members [eff !! err, Error BootError] r =>
  InterpreterFor eff r
resumeBootError =
  resumeHoistError @_ @eff (BootError . show @Text)
