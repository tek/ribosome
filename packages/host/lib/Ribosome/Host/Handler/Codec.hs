{-# options_haddock prune #-}

module Ribosome.Host.Handler.Codec where

import Data.Aeson (eitherDecodeStrict')
import qualified Data.ByteString as ByteString
import Data.MessagePack (Object)
import qualified Data.Text as Text
import Exon (exon)
import qualified Options.Applicative as Optparse
import Options.Applicative (defaultPrefs, execParserPure, info, renderFailure)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Host.Data.Args (ArgList (ArgList), Args (Args), JsonArgs (JsonArgs), OptionParser (optionParser), Options (Options))
import Ribosome.Host.Data.Bang (Bang (NoBang))
import Ribosome.Host.Data.Bar (Bar (Bar))
import Ribosome.Host.Data.Report (Report, basicReport)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandlerFun)

decodeArg ::
  Member (Stop Report) r =>
  MsgpackDecode a =>
  Object ->
  Sem r a
decodeArg =
  stopEither . first fromText . fromMsgpack

extraError ::
  Member (Stop Report) r =>
  [Object] ->
  Sem r a
extraError o =
  stop (fromString [exon|Extraneous arguments: #{show o}|])

optArg ::
  Member (Stop Report) r =>
  MsgpackDecode a =>
  a ->
  [Object] ->
  Sem r ([Object], a)
optArg dflt = \case
  [] -> pure ([], dflt)
  (o : rest) -> do
    a <- decodeArg o
    pure (rest, a)

-- |This class is used by 'HandlerCodec' to decode handler function parameters.
-- Each parameter may consume zero or arbitrarily many of the RPC message's arguments.
--
-- Users may create instances for their types to implement custom decoding, especially for commands, since those don't
-- have structured arguments.
--
-- See also 'Ribosome.CommandHandler'.
class HandlerArg a r where
  -- Take an arbitrary number of arguments from the list and return a value of type @a@ as well as the remaining
    -- arguments.
  handlerArg :: [Object] -> Sem r ([Object], a)

instance {-# overlappable #-} (
    Member (Stop Report) r,
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
    Member (Stop Report) r
  ) => HandlerArg Bang r where
    handlerArg =
      optArg NoBang

instance (
    Member (Stop Report) r
  ) => HandlerArg Args r where
  handlerArg os =
    case traverse fromMsgpack os of
      Right a ->
        pure ([], Args (Text.unwords a))
      Left e ->
        basicReport [exon|Invalid arguments: #{show os}|] ["Invalid type for Args", show os, e]

instance (
    Member (Stop Report) r
  ) => HandlerArg ArgList r where
  handlerArg os =
    case traverse fromMsgpack os of
      Right a ->
        pure ([], ArgList a)
      Left e ->
        basicReport [exon|Invalid arguments: #{show os}|] ["Invalid type for ArgList", show os, e]

instance (
    Member (Stop Report) r,
    FromJSON a
  ) => HandlerArg (JsonArgs a) r where
  handlerArg os =
    case first toText . eitherDecodeStrict' . ByteString.concat =<< traverse fromMsgpack os of
      Right a ->
        pure ([], JsonArgs a)
      Left e ->
        basicReport [exon|Invalid arguments: #{show os}|] ["Invalid type for JsonArgs", show os, e]

instance (
    Member (Stop Report) r,
    OptionParser a
  ) => HandlerArg (Options a) r where
  handlerArg os =
    case result . execParserPure defaultPrefs (info (optionParser @a) mempty) =<< traverse fromMsgpack os of
      Right a ->
        pure ([], Options a)
      Left e ->
        basicReport [exon|Invalid arguments: #{show os}|] ["Invalid type for Options", show os, e]
    where
      result = \case
        Optparse.Success a -> Right a
        Optparse.Failure e -> Left (toText (fst (renderFailure e "Ribosome")))
        Optparse.CompletionInvoked _ -> Left "Internal optparse error"

-- |The class of functions that can be converted to canonical RPC handlers of type 'RpcHandlerFun'.
class HandlerCodec h r | h -> r where
  -- |Convert a type containing a 'Sem' to a canonicalized 'RpcHandlerFun' by transforming each function parameter with
  -- 'HandlerArg'.
  handlerCodec :: h -> RpcHandlerFun r

instance (
    MsgpackEncode a
  ) => HandlerCodec (Handler r a) r where
    handlerCodec h = \case
      [] -> toMsgpack <$> h
      o -> extraError o

instance (
    HandlerArg a (Stop Report : r),
    HandlerCodec b r
  ) => HandlerCodec (a -> b) r where
  handlerCodec h o = do
    (rest, a) <- handlerArg o
    handlerCodec (h a) rest
