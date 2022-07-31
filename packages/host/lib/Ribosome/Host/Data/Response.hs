module Ribosome.Host.Data.Response where

import Data.MessagePack (Object)
import Exon (exon)

import Ribosome.Host.Data.Request (RequestId (RequestId))

data Response =
  Success Object
  |
  Error Text
  deriving stock (Eq, Show)

formatResponse :: Response -> Text
formatResponse = \case
  Success o -> show o
  Error e -> [exon|error: #{e}|]

data TrackedResponse =
  TrackedResponse {
    id :: RequestId,
    payload :: Response
  }
  deriving stock (Eq, Show)

formatTrackedResponse :: TrackedResponse -> Text
formatTrackedResponse (TrackedResponse (RequestId i) payload) =
  [exon|<#{show i}> #{formatResponse payload}|]
