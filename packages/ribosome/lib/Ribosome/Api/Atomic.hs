module Ribosome.Api.Atomic where

import Data.MessagePack (Object(ObjectNil, ObjectArray, ObjectString))
import Neovim.Plugin.Classes (FunctionName(F))
import qualified Ribosome.Nvim.Api.RpcCall as RpcError (RpcError(Atomic))

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Msgpack.Decode (MsgpackDecode(..), fromMsgpack')
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Msgpack.Error (DecodeError)
import qualified Ribosome.Msgpack.Util as Util (illegalType)
import Ribosome.Nvim.Api.IO (nvimCallAtomic)
import Ribosome.Nvim.Api.RpcCall (RpcCall(RpcCall))

data AtomicStatus =
  Failure Text
  |
  Success
  deriving (Eq, Show)

instance MsgpackDecode AtomicStatus where
  fromMsgpack ObjectNil =
    Right Success
  fromMsgpack (ObjectArray [_, _, ObjectString msg]) =
    Right (Failure (decodeUtf8 msg))
  fromMsgpack o =
    Util.illegalType "AtomicStatus" o

-- |Bundle a list of 'RpcCall's into a single call to nvim_call_atomic.
-- The result is checked for an error message, and if it is present, the call will fail.
atomic ::
  MonadDeepError e DecodeError m =>
  NvimE e m =>
  [RpcCall] ->
  m [Object]
atomic calls = do
  (results, statusObject) <- unpack =<< nvimCallAtomic (call <$> calls)
  status <- fromMsgpack' statusObject
  check results status
  where
    call (RpcCall (F name) args) =
      toMsgpack @[Object] [toMsgpack name, toMsgpack args]
    unpack [ObjectArray results, status] =
      return (results, status)
    unpack o =
      throwHoist (RpcError.Atomic ("unexpected result structure: " <> show o))
    check _ (Failure err) =
      throwHoist (RpcError.Atomic err)
    check results Success =
      return results

-- |Bundle calls into one and decode all results to the same type.
atomicAs ::
  MonadDeepError e DecodeError m =>
  MsgpackDecode a =>
  NvimE e m =>
  [RpcCall] ->
  m [a]
atomicAs =
  traverse fromMsgpack' <=< atomic
