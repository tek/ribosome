module Ribosome.Host.Effect.Rpc where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Data.Request (Request)
import Ribosome.Host.Data.RpcError (RpcError)

data Rpc :: Effect where
  Sync :: MsgpackDecode a => Request a -> Rpc m a
  Async :: MsgpackDecode a => Request a -> (Either RpcError a -> m ()) -> Rpc m ()
  Notify :: Request a -> Rpc m ()

makeSem ''Rpc
