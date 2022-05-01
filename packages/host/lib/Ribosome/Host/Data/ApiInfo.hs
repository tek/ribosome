module Ribosome.Host.Data.ApiInfo where

import Data.MessagePack (Object)
import qualified Data.Serialize as Serialize
import System.Process.Typed (proc, readProcessStdout_)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))
import Ribosome.Host.Data.ApiType (ApiType)

data RpcDecl =
  RpcDecl {
    name :: String,
    parameters :: [(ApiType, String)],
    since :: Maybe Int64,
    deprecated_since :: Maybe Int64,
    method :: Bool,
    return_type :: ApiType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode)

newtype ExtType =
  ExtType { unExtType :: String }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, MsgpackDecode)

data ExtTypeMeta =
  ExtTypeMeta {
    id :: Int64,
    prefix :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode)

data ApiInfo =
  ApiInfo {
    types :: Map ExtType ExtTypeMeta,
    functions :: [RpcDecl]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode)

msgpack :: IO (Either Text Object)
msgpack =
    first toText . Serialize.decode . toStrict <$> readProcessStdout_ (proc "nvim" ["--api-info"])

apiInfo :: IO (Either Text ApiInfo)
apiInfo = (fromMsgpack =<<) <$> msgpack
