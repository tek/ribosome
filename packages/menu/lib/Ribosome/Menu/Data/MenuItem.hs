module Ribosome.Menu.Data.MenuItem where

import qualified Data.IntMap.Strict as IntMap

data MenuItem a =
  MenuItem {
    meta :: a,
    text :: Text,
    render :: Text
  }
  deriving stock (Eq, Show, Ord, Functor, Generic)

type Items a =
  IntMap (MenuItem a)

simpleMenuItem :: a -> Text -> MenuItem a
simpleMenuItem a t =
  MenuItem a t t

simpleItems :: [Text] -> Items ()
simpleItems =
  IntMap.fromList . zip [0..] . fmap (simpleMenuItem ())

intItems :: [Int] -> Items Int
intItems nums =
  IntMap.fromList [(i, simpleMenuItem i (show i)) | i <- nums]
