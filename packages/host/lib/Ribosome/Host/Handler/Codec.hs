module Ribosome.Host.Handler.Codec where

import Data.MessagePack (Object)
import Exon (exon)

import Ribosome.Host.Class.Msgpack.Decode (pattern Msgpack, MsgpackDecode (fromMsgpack))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Host.Data.Args (Args (Args))
import Ribosome.Host.Data.Bang (Bang(NoBang))
import Ribosome.Host.Data.Bar (Bar (Bar))
import qualified Ribosome.Host.Data.HandlerError as HandlerError
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandlerFun)

decodeArg ::
  Member (Stop HandlerError) r =>
  MsgpackDecode a =>
  Object ->
  Sem r a
decodeArg =
  stopEither . first HandlerError.simple . fromMsgpack

extraError ::
  Member (Stop HandlerError) r =>
  [Object] ->
  Sem r a
extraError o =
  stop (HandlerError.simple [exon|Extraneous arguments: #{show o}|])

optArg ::
  Member (Stop HandlerError) r =>
  MsgpackDecode a =>
  a ->
  [Object] ->
  Sem r ([Object], a)
optArg dflt = \case
  [] -> pure ([], dflt)
  (o : rest) -> do
    a <- decodeArg o
    pure (rest, a)

class HandlerArg a r where
  handlerArg :: [Object] -> Sem r ([Object], a)

instance {-# overlappable #-} (
    Member (Stop HandlerError) r,
    MsgpackDecode a
  ) => HandlerArg a r where
    handlerArg = \case
      [] -> stop "too few arguments"
      (o : rest) -> do
        a <- decodeArg o
        pure (rest, a)

instance (
    Member (Stop HandlerError) r,
    MsgpackDecode a
  ) => HandlerArg (Maybe a) r where
    handlerArg =
      optArg Nothing

instance HandlerArg Bar r where
  handlerArg os =
    pure (os, Bar)

instance (
    Member (Stop HandlerError) r
  ) => HandlerArg Bang r where
    handlerArg =
      optArg NoBang

instance (
    Member (Stop HandlerError) r
  ) => HandlerArg Args r where
  handlerArg = \case
    [] -> stop "Too few arguments"
    (Msgpack a : rest) ->
      pure (rest, Args a)
    a : _ ->
      stop (HandlerError.simple [exon|Invalid type for Args: #{show a}|])

class HandlerCodec h r | h -> r where
  handlerCodec :: h -> RpcHandlerFun r

instance (
    MsgpackEncode a
  ) => HandlerCodec (Handler r a) r where
    handlerCodec h = \case
      [] -> toMsgpack <$> h
      o -> extraError o

instance (
    HandlerArg a (Stop HandlerError : r),
    HandlerCodec b r
  ) => HandlerCodec (a -> b) r where
  handlerCodec h o = do
    (rest, a) <- handlerArg o
    handlerCodec (h a) rest
