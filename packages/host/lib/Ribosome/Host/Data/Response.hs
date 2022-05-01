module Ribosome.Host.Data.Response where

import Ribosome.Host.Data.Request (RequestId)
import Ribosome.Host.Data.RpcError (RpcError)

data Response a =
  Success a
  |
  Error RpcError
  deriving stock (Eq, Show)

data TrackedResponse a =
  TrackedResponse {
    id :: RequestId,
    payload :: Response a
  }
  deriving stock (Eq, Show)
