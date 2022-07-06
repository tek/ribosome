module Ribosome.Data.FloatOptions where

import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Host.Class.Msgpack.Map (msgpackMap)

data FloatRelative =
  Editor
  |
  Win
  |
  Cursor
  deriving stock (Eq, Show)

instance MsgpackEncode FloatRelative where
  toMsgpack Editor = toMsgpack ("editor" :: Text)
  toMsgpack Win = toMsgpack ("win" :: Text)
  toMsgpack Cursor = toMsgpack ("cursor" :: Text)

instance Default FloatRelative where
  def = Cursor

data FloatAnchor =
  NW
  |
  NE
  |
  SW
  |
  SE
  deriving stock (Eq, Show)

instance MsgpackEncode FloatAnchor where
  toMsgpack NW = toMsgpack ("NW" :: Text)
  toMsgpack NE = toMsgpack ("NE" :: Text)
  toMsgpack SW = toMsgpack ("SW" :: Text)
  toMsgpack SE = toMsgpack ("SE" :: Text)

instance Default FloatAnchor where
  def = NW

data FloatBorder =
  None
  |
  Single
  |
  Double
  |
  Rounded
  |
  Solid
  |
  Shadow
  |
  Manual [Text]
  deriving stock (Eq, Show, Generic)

instance MsgpackEncode FloatBorder where
  toMsgpack = \case
    None -> toMsgpack @Text "none"
    Single -> toMsgpack @Text "single"
    Double -> toMsgpack @Text "double"
    Rounded -> toMsgpack @Text "rounded"
    Solid -> toMsgpack @Text "solid"
    Shadow -> toMsgpack @Text "shadow"
    Manual cs -> toMsgpack cs

instance Default FloatBorder where
  def =
    Rounded

data FloatStyle =
  FloatStyleMinimal
  deriving stock (Eq, Show)

instance Default FloatStyle where
  def =
    FloatStyleMinimal

instance MsgpackEncode FloatStyle where
  toMsgpack FloatStyleMinimal =
    toMsgpack @Text "minimal"

newtype FloatZindex =
  FloatZindex { unFloatZindex :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

instance MsgpackEncode FloatZindex where
  toMsgpack (FloatZindex i) =
    toMsgpack i

data FloatOptions =
  FloatOptions {
    relative :: FloatRelative,
    width :: Int,
    height :: Int,
    row :: Int,
    col :: Int,
    focusable :: Bool,
    anchor :: FloatAnchor,
    bufpos :: Maybe (Int, Int),
    border :: FloatBorder,
    noautocmd :: Bool,
    enter :: Bool,
    style :: Maybe FloatStyle,
    zindex :: Maybe FloatZindex
  }
  deriving stock (Eq, Show, Generic)

instance MsgpackEncode FloatOptions where
  toMsgpack FloatOptions {..} =
    msgpackMap
    ("relative", relative)
    ("width", width)
    ("height", height)
    ("row", row)
    ("col", col)
    ("focusable", focusable)
    ("anchor", anchor)
    ("bufpos", bufpos)
    ("border", border)
    ("noautocmd", noautocmd)
    ("style", style)
    ("zindex", zindex)

instance Default FloatOptions where
  def =
    FloatOptions def 30 10 1 1 False def def def False True (Just FloatStyleMinimal) Nothing
