module Ribosome.Host.RpcError where

import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)

ignoreRpcError ::
  Member (Rpc !! RpcError) r =>
  Sem (Rpc : r) a ->
  Sem r ()
ignoreRpcError =
  resume_ . void
