module Ribosome.Host.Handler.Codec where

import Data.MessagePack (Object)
import Exon (exon)

import Ribosome.Host.Class.Msgpack.Decode (pattern Msgpack, MsgpackDecode (fromMsgpack))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Host.Data.Args (Args (Args))
import Ribosome.Host.Data.Bar (Bar (Bar))
import Ribosome.Host.Data.HandlerError (HandlerError (HandlerError))
import Ribosome.Host.Data.RpcHandler (RpcHandlerFun)

decodeArg ::
  Member (Error HandlerError) r =>
  MsgpackDecode a =>
  Object ->
  Sem r a
decodeArg =
  fromEither . first HandlerError . fromMsgpack

extraError ::
  Member (Error HandlerError) r =>
  [Object] ->
  Sem r a
extraError o =
  throw (HandlerError [exon|Extraneous arguments: #{show o}|])

class HandlerArg a r where
  handlerArg :: [Object] -> Sem r ([Object], a)

instance {-# overlappable #-} (
    Member (Error HandlerError) r,
    MsgpackDecode a
  ) => HandlerArg a r where
  handlerArg = \case
    [] -> throw "too few arguments"
    (o : rest) -> do
      a <- decodeArg o
      pure (rest, a)

instance HandlerArg Bar r where
  handlerArg os =
    pure (os, Bar)

instance (
    Member (Error HandlerError) r
  ) => HandlerArg Args r where
  handlerArg = \case
    [] -> throw "Too few arguments"
    (Msgpack a : rest) ->
      pure (rest, Args a)
    a : _ ->
      throw (HandlerError [exon|Invalid type for Args: #{show a}|])

class HandlerCodec h r where
  handlerCodec :: h -> RpcHandlerFun r

instance (
    MsgpackEncode a
  ) => HandlerCodec (Sem (Error HandlerError : r) a) r where
    handlerCodec h = \case
      [] -> toMsgpack <$> h
      o -> extraError o

instance {-# overlappable #-} (
    MsgpackEncode a
  ) => HandlerCodec a r where
    handlerCodec a = \case
      [] -> pure (toMsgpack a)
      o -> extraError o

instance (
    HandlerArg a (Error HandlerError : r),
    HandlerCodec b r
  ) => HandlerCodec (a -> b) r where
  handlerCodec h o = do
    (rest, a) <- handlerArg @a @(Error HandlerError : r) o
    handlerCodec (h a) rest
