module Ribosome.Host.Effect.Handlers where

import Data.MessagePack (Object)

import Ribosome.Host.Data.Request (RpcMethod)

data Handlers :: Effect where
  Register :: Handlers m ()
  Run :: RpcMethod -> [Object] -> Handlers m (Maybe Object)

makeSem ''Handlers
