module Ribosome.Data.Mapping where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)

newtype MappingIdent =
  MappingIdent { unMappingIdent :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, MsgpackDecode, MsgpackEncode)

data Mapping =
  Mapping {
    mappingIdent :: MappingIdent,
    mappingLhs :: Text,
    mappingMode :: Text,
    mappingRemap :: Bool,
    mappingBuffer :: Bool
  }
  deriving stock (Eq, Show)
