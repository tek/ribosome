module Ribosome.Host.Class.Msgpack.Map where

import Data.MessagePack (Object)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))

class MsgpackMapElem a where
  msgpackMapElem :: a -> Map Text Object

instance {-# overlappable #-} (
    MsgpackEncode a,
    t ~ Text
  ) => MsgpackMapElem (t, a) where
    msgpackMapElem (k, v) =
      [(k, toMsgpack v)]

instance (
    MsgpackEncode a,
    t ~ Text
  ) => MsgpackMapElem (t, Maybe a) where
    msgpackMapElem = \case
      (k, Just v) ->
        [(k, toMsgpack v)]
      (_, Nothing) ->
        mempty

class MsgpackMap a where
  msgpackMap :: a

instance MsgpackMap (Map Text Object -> Object) where
  msgpackMap =
    toMsgpack

instance MsgpackMap (a -> a) where
  msgpackMap =
    id

instance (
    MsgpackMapElem (t, a),
    MsgpackMap (Map Text Object -> b)
  ) => MsgpackMap (Map Text Object -> (t, a) -> b) where
  msgpackMap m a =
    msgpackMap (m <> msgpackMapElem a)

instance (
    MsgpackMapElem (t, a),
    MsgpackMap (Map Text Object -> b)
  ) => MsgpackMap ((t, a) -> b) where
  msgpackMap a =
    msgpackMap (msgpackMapElem a)
