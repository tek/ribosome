module Ribosome.Data.WindowView where

import Ribosome.Msgpack.Decode (MsgpackDecode)
import Ribosome.Msgpack.Encode (MsgpackEncode)

data WindowView =
  WindowView {
    lnum :: Int,
    topline :: Int
  }
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode)

data PartialWindowView =
  PartialWindowView {
    lnum :: Maybe Int,
    topline :: Maybe Int
  }
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode)

class AsPartialWindowView a where
  asPartialWindowView :: a -> PartialWindowView

instance AsPartialWindowView WindowView where
  asPartialWindowView (WindowView l t) =
    PartialWindowView (Just l) (Just t)

instance AsPartialWindowView PartialWindowView where
  asPartialWindowView =
    id
