module Ribosome.Host.Data.Response where

import Data.MessagePack (Object)

import Ribosome.Host.Data.Request (RequestId)
import Ribosome.Host.Data.RpcError (RpcError)

data Response =
  Success Object
  |
  Error RpcError
  deriving stock (Eq, Show)

data TrackedResponse =
  TrackedResponse {
    id :: RequestId,
    payload :: Response
  }
  deriving stock (Eq, Show)
