module Ribosome.Host.Data.Response where

import Data.MessagePack (Object)
import Exon (exon)

import Ribosome.Host.Data.Request (RequestId (RequestId))
import qualified Ribosome.Host.Data.RpcError as RpcError
import Ribosome.Host.Data.RpcError (RpcError)

data Response =
  Success Object
  |
  Error RpcError
  deriving stock (Eq, Show)

formatResponse :: Response -> Text
formatResponse = \case
  Success o -> show o
  Error (RpcError.Decode e) -> [exon|decode error: #{e}|]
  Error (RpcError.Unexpected e) -> [exon|error: #{e}|]

data TrackedResponse =
  TrackedResponse {
    id :: RequestId,
    payload :: Response
  }
  deriving stock (Eq, Show)

formatTrackedResponse :: TrackedResponse -> Text
formatTrackedResponse (TrackedResponse (RequestId i) payload) =
  [exon|<#{show i}> #{formatResponse payload}|]
