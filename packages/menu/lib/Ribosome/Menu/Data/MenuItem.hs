module Ribosome.Menu.Data.MenuItem where

import qualified Data.IntMap.Strict as IntMap

data MenuItem a =
  MenuItem {
    meta :: a,
    text :: Text,
    truncated :: Text
  }
  deriving stock (Eq, Show, Ord, Functor, Generic)

type Items a =
  IntMap (MenuItem a)

simpleMenuItem :: a -> Text -> MenuItem a
simpleMenuItem a t =
  MenuItem a t t

intItems :: [Int] -> Items Int
intItems nums =
  IntMap.fromList [(i, simpleMenuItem i (show i)) | i <- nums]
