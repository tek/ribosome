-- |Codec data type for @winsaveview@.
module Ribosome.Data.WindowView where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)

-- |Codec data type for @winsaveview@.
data WindowView =
  WindowView {
    lnum :: Int,
    topline :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode, MsgpackDecode)

-- |Codec data type for @winrestview@.
data PartialWindowView =
  PartialWindowView {
    lnum :: Maybe Int,
    topline :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode, MsgpackDecode)

-- |Convert between the return type of @winsaveview@ and the parameter type of @winrestview@.
class AsPartialWindowView a where
  asPartialWindowView :: a -> PartialWindowView

instance AsPartialWindowView WindowView where
  asPartialWindowView (WindowView l t) =
    PartialWindowView (Just l) (Just t)

instance AsPartialWindowView PartialWindowView where
  asPartialWindowView =
    id
