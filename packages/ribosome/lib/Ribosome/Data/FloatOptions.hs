-- |Data types for floating window API codec.
module Ribosome.Data.FloatOptions where

import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Host.Class.Msgpack.Map (msgpackMap)

-- |The reference point to which a floating window's position is defined.
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

-- |The corner of a floating window that is positioned at the specified coordinates.
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

-- |The border style of a floating window.
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
  -- |A list of characters that is drawn for the border, starting with the top left corner, going clockwise, repeating
  -- if too short.
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

-- |Neovim has a style option for floating windows that sets a few options in bulk, with only one possible value.
data FloatStyle =
  FloatStyleMinimal
  deriving stock (Eq, Show)

instance Default FloatStyle where
  def =
    FloatStyleMinimal

instance MsgpackEncode FloatStyle where
  toMsgpack FloatStyleMinimal =
    toMsgpack @Text "minimal"

-- |The z-index of a floating window, determining occlusion.
newtype FloatZindex =
  FloatZindex { unFloatZindex :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

instance MsgpackEncode FloatZindex where
  toMsgpack (FloatZindex i) =
    toMsgpack i

-- |The set of options accepted by the @float@ key of the argument to @nvim_open_win@, configuring the appearance and
-- geometry of a floating window.
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
