module Ribosome.Data.FloatOptions where

import qualified Data.Map as Map

import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))

data FloatRelative =
  Editor
  |
  Win
  |
  Cursor
  deriving (Eq, Show)

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
  deriving (Eq, Show)

instance MsgpackEncode FloatAnchor where
  toMsgpack NW = toMsgpack ("NW" :: Text)
  toMsgpack NE = toMsgpack ("NE" :: Text)
  toMsgpack SW = toMsgpack ("SW" :: Text)
  toMsgpack SE = toMsgpack ("SE" :: Text)

instance Default FloatAnchor where
  def = NW

data FloatOptions =
  FloatOptions {
    relative :: FloatRelative,
    width :: Int,
    height :: Int,
    row :: Int,
    col :: Int,
    focusable :: Bool,
    anchor :: FloatAnchor,
    bufpos :: Maybe (Int, Int)
  }
  deriving (Eq, Show, Generic)

instance MsgpackEncode FloatOptions where
  toMsgpack FloatOptions {..} =
    toMsgpack $ Map.fromList (simple ++ maybe [] (pure . ("bufpos",) . toMsgpack) bufpos)
    where
      simple =
        [
          ("relative" :: Text, toMsgpack relative),
          ("width", toMsgpack width),
          ("height", toMsgpack height),
          ("row", toMsgpack row),
          ("col", toMsgpack col),
          ("focusable", toMsgpack focusable),
          ("anchor", toMsgpack anchor),
          ("relative", toMsgpack relative)
        ]

instance Default FloatOptions where
  def =
    FloatOptions def 30 10 1 1 False def def
