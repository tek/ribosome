module Ribosome.Data.Mapping where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)
import Ribosome.Host.Data.RpcName (RpcName)

newtype MappingId =
  MappingId { unMappingId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, MsgpackDecode, MsgpackEncode)

data Mapping =
  Mapping {
    rpc :: RpcName,
    lhs :: Text,
    mode :: Text,
    id :: Maybe MappingId
  }
  deriving stock (Eq, Show)
