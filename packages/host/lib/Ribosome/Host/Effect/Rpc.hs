module Ribosome.Host.Effect.Rpc where

import Ribosome.Host.Data.RpcCall (RpcCall)
import Ribosome.Host.Data.RpcError (RpcError)

data Rpc :: Effect where
  Sync :: RpcCall a -> Rpc m a
  Async :: RpcCall a -> (Either RpcError a -> m ()) -> Rpc m ()
  Notify :: RpcCall a -> Rpc m ()

makeSem ''Rpc
