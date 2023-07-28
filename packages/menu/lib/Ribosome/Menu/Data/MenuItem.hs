module Ribosome.Menu.Data.MenuItem (
  module Ribosome.Menu.Data.MenuItem,
  MenuItem (MenuItem, ..),
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text

data MenuItem a =
  UnsafeMenuItem {
    meta :: a,
    text :: Text,
    lines :: Word,
    render :: NonEmpty Text
  }
  deriving stock (Eq, Show, Ord, Functor, Generic)

pattern MenuItem :: a -> Text -> NonEmpty Text -> MenuItem a
pattern MenuItem {meta, text, render} <- UnsafeMenuItem {..}
  where
    MenuItem meta text render = UnsafeMenuItem {lines = fromIntegral (length render), ..}

{-# complete MenuItem #-}

type Items a =
  IntMap (MenuItem a)

simpleMenuItem :: a -> Text -> MenuItem a
simpleMenuItem a t =
  MenuItem a t [t]

simpleMenuItemLines :: a -> NonEmpty Text -> MenuItem a
simpleMenuItemLines a t =
  MenuItem a (Text.intercalate "\n" (toList t)) t

simpleItems :: [Text] -> Items ()
simpleItems =
  IntMap.fromList . zip [0..] . fmap (simpleMenuItem ())

intItems :: [Int] -> Items Int
intItems nums =
  IntMap.fromList [(i, simpleMenuItem i (show i)) | i <- nums]
