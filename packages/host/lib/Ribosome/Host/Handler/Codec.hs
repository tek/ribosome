module Ribosome.Host.Handler.Codec where

import Data.MessagePack (Object)
import qualified Data.Text as Text
import Exon (exon)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Host.Data.Args (ArgList (ArgList), Args (Args))
import Ribosome.Host.Data.Bang (Bang (NoBang))
import Ribosome.Host.Data.Bar (Bar (Bar))
import qualified Ribosome.Host.Data.HandlerError as HandlerError
import Ribosome.Host.Data.HandlerError (HandlerError, basicHandlerError)
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
    HandlerArg a r
  ) => HandlerArg (Maybe a) r where
    handlerArg = \case
      [] -> pure ([], Nothing)
      os -> second Just <$> handlerArg os

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
  handlerArg os =
    case traverse fromMsgpack os of
      Right a ->
        pure ([], Args (Text.unwords a))
      Left e ->
        basicHandlerError [exon|Invalid arguments: #{show os}|] ["Invalid type for Args", show os, e]

instance (
    Member (Stop HandlerError) r
  ) => HandlerArg ArgList r where
  handlerArg os =
    case traverse fromMsgpack os of
      Right a ->
        pure ([], ArgList a)
      Left e ->
        basicHandlerError [exon|Invalid arguments: #{show os}|] ["Invalid type for ArgList", show os, e]

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
