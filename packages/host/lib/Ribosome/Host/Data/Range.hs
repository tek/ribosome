module Ribosome.Host.Data.Range where

import Data.MessagePack (Object (ObjectArray))
import Exon (exon)

import Ribosome.Host.Class.Msgpack.Decode (pattern Msgpack, MsgpackDecode (fromMsgpack))

data RangeStyle =
  RangeFile
  |
  RangeLine (Maybe Nat)
  |
  RangeCount (Maybe Nat)

data Range (style :: RangeStyle) =
  Range {
    low :: Int64,
    high :: Maybe Int64
  }
  deriving stock (Eq, Show)

instance MsgpackDecode (Range style) where
  fromMsgpack = \case
    ObjectArray [Msgpack low, Msgpack high] ->
      Right (Range low (Just high))
    ObjectArray [Msgpack low] ->
      Right (Range low Nothing)
    o ->
      Left [exon|Range must be an array with one or two elements: #{show o}|]

class RangeStyleOpt (s :: RangeStyle) where
  rangeStyleOpt :: Text

  rangeStyleArg :: Text
  rangeStyleArg =
    "[<line1>, <line2>]"

instance RangeStyleOpt ('RangeLine 'Nothing) where
  rangeStyleOpt =
    "-range"

instance RangeStyleOpt 'RangeFile where
  rangeStyleOpt =
    "-range=%"

instance (
    KnownNat n
  ) => RangeStyleOpt ('RangeLine ('Just n)) where
  rangeStyleOpt =
    [exon|-range=#{show (natVal (Proxy @n))}|]

  rangeStyleArg =
    "[<count>]"

instance RangeStyleOpt ('RangeCount 'Nothing) where
  rangeStyleOpt =
    "-count"

  rangeStyleArg =
    "[<count>]"

instance (
    KnownNat n
  ) => RangeStyleOpt ('RangeCount ('Just n)) where
  rangeStyleOpt =
    [exon|-count=#{show (natVal (Proxy @n))}|]

  rangeStyleArg =
    "[<count>]"
