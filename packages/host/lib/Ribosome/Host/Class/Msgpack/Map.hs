-- |Helper for encoding values to a heterogeneous MessagePack map.
module Ribosome.Host.Class.Msgpack.Map where

import Data.MessagePack (Object)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))

-- |Utility class for 'MsgpackMap'.
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

-- |This class provides a variadic method for encoding MessagePack maps.
class MsgpackMap a where
  -- |Encode an arbitrary number of heterogeneously typed values to a single MessagePack map.
  -- This function is variadic, meaning that it takes an arbitrary number of arguments:
  --
  -- >>> msgpackMap ("number", 5 :: Int) ("status", "error" :: Text) ("intensity", 3.14 :: Double) :: Object
  -- ObjectMap (Map.fromList [(ObjectString "number", ObjectInt 5), (ObjectString "status", ObjectString "error"), (ObjectString "intensity", ObjectFloat 3.14)])
  --
  -- This avoids the need to call 'Ribosome.toMsgpack' once for each element and then once more for the map.
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
