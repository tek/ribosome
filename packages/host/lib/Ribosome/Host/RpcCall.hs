{-# options_haddock prune #-}

-- |Conversion of 'RpcCall' to 'RpcBatch'
module Ribosome.Host.RpcCall where

import Data.MessagePack (Object (ObjectArray, ObjectNil))
import Exon (exon)

import Ribosome.Host.Class.Msgpack.Decode (pattern Msgpack)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Class.Msgpack.Error (DecodeError (DecodeError), FieldError (FieldError))
import Ribosome.Host.Data.Request (Request (Request))
import qualified Ribosome.Host.Data.RpcBatch as RpcBatch
import Ribosome.Host.Data.RpcBatch (RpcBatch)
import Ribosome.Host.Data.RpcCall (RpcCall (..))

atomicError ::
  Text ->
  Either DecodeError a
atomicError msg =
  Left (DecodeError "atomic call response" (FieldError msg))

checkLeftovers :: ([Object], a) -> Either DecodeError a
checkLeftovers = \case
  ([], a) ->
    Right a
  (res, _) ->
    atomicError [exon|Excess results: #{show res}|]

atomicRequest :: [Request] -> Request
atomicRequest reqs =
  Request "nvim_call_atomic" [toMsgpack reqs]

atomicResult ::
  ([Object] -> Either DecodeError ([Object], a)) ->
  Object ->
  Either DecodeError a
atomicResult decode = \case
  ObjectArray [Msgpack res, ObjectNil] ->
    checkLeftovers =<< decode res
  ObjectArray [_, errs] ->
    atomicError (show errs)
  o ->
    atomicError [exon|Not an array: #{show o}|]

-- |Create calls to @nvim_call_atomic@ from 'RpcAtomic' requests and unify their constructor with 'RpcRequest'.
cata :: RpcCall a -> RpcBatch a
cata = \case
  RpcPure a ->
    RpcBatch.Pure a
  RpcRequest req decode ->
    RpcBatch.Request req decode
  RpcBind fa f ->
    RpcBatch.Bind (cata fa) (cata . f)
  RpcAtomic reqs decode ->
    RpcBatch.Request (atomicRequest reqs) (atomicResult decode)
