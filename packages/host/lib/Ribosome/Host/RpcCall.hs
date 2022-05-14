module Ribosome.Host.RpcCall where

import Data.MessagePack (Object (ObjectArray, ObjectNil))
import Exon (exon)

import Ribosome.Host.Class.Msgpack.Decode (pattern Msgpack, MsgpackDecode (fromMsgpack))
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.Request (Request (Request))
import Ribosome.Host.Data.RpcCall (RpcCall (RpcAtomic, RpcCallRequest, RpcFmap, RpcPure))

decodeAtom ::
  MsgpackDecode a =>
  [Object] ->
  Either Text ([Object], a)
decodeAtom = \case
  o : rest ->
    (rest,) <$> fromMsgpack o
  [] ->
    Left "Too few results in atomic call response"

foldAtomic :: RpcCall a -> ([Request], [Object] -> Either Text ([Object], a))
foldAtomic = \case
  RpcCallRequest req ->
    ([coerce req], decodeAtom)
  RpcPure a ->
    ([], Right . (,a))
  RpcFmap f a ->
    second (second (second f) .) (foldAtomic a)
  RpcAtomic f aa ab ->
    (reqsA <> reqsB, decode)
    where
      decode o = do
        (restA, a) <- decodeA o
        second (f a) <$> decodeB restA
      (reqsB, decodeB) =
        foldAtomic ab
      (reqsA, decodeA) =
        foldAtomic aa

checkLeftovers :: ([Object], a) -> Either Text a
checkLeftovers = \case
  ([], a) -> Right a
  (res, _) -> Left [exon|Excess results in atomic call response: #{show res}|]

atomicRequest :: [Request] -> Request
atomicRequest reqs =
  Request "nvim_call_atomic" [toMsgpack reqs]

atomicResult ::
  ([Object] -> Either Text ([Object], a)) ->
  Object ->
  Either Text a
atomicResult decode = \case
  ObjectArray [Msgpack res, ObjectNil] ->
    checkLeftovers =<< decode res
  ObjectArray [_, errs] ->
    Left (show errs)
  o ->
    Left ("Bad atomic result: " <> show o)

cata :: RpcCall a -> Either a (Request, Object -> Either Text a)
cata = \case
  RpcCallRequest req ->
    Right (req, fromMsgpack)
  RpcPure a ->
    Left a
  RpcFmap f a ->
    bimap f (second (second f .)) (cata a)
  a@RpcAtomic {} ->
    Right (bimap atomicRequest atomicResult (foldAtomic a))
