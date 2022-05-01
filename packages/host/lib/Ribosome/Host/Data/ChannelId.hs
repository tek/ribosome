module Ribosome.Host.Data.ChannelId where
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))

newtype ChannelId =
  ChannelId { unChannelId :: Int64 }
  deriving stock (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord)

instance MsgpackDecode ChannelId where
  fromMsgpack =
    fmap ChannelId . fromMsgpack
