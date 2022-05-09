module Ribosome.Host.Handler where

import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Host.Data.Execution (Execution)
import Ribosome.Host.Data.HandlerError (HandlerError (HandlerError))
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler), RpcHandlerFun)
import qualified Ribosome.Host.Data.RpcType as RpcType
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))
import Data.MessagePack (Object)
import Exon (exon)

decodeArg ::
  Member (Error HandlerError) r =>
  MsgpackDecode a =>
  Object ->
  Sem r a
decodeArg =
  fromEither . first HandlerError . fromMsgpack

class FunctionHandler h r where
  functionHandler :: h -> RpcHandlerFun r

instance (
    MsgpackEncode a
  ) => FunctionHandler (Sem (Error HandlerError : r) a) r where
    functionHandler h = \case
      [] -> toMsgpack <$> h
      o -> throw (HandlerError [exon|Extraneous arguments: #{show o}|])

instance (
    MsgpackDecode a,
    FunctionHandler b r
  ) => FunctionHandler (a -> b) r where
  functionHandler h = \case
    [] -> throw "too few arguments"
    (o : rest) -> do
      a <- decodeArg o
      functionHandler (h a) rest

rpcFunction :: FunctionHandler h r => Text -> Execution -> h -> RpcHandler r
rpcFunction name execution h =
  RpcHandler RpcType.Function name execution (functionHandler h)
