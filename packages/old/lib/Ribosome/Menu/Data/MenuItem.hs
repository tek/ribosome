module Ribosome.Menu.Data.MenuItem where
import qualified Data.IntMap.Strict as IntMap

data MenuItem a =
  MenuItem {
    _meta :: a,
    _text :: Text,
    _truncated :: Text
  }
  deriving stock (Eq, Show, Ord, Functor)

makeClassy ''MenuItem

type Items a =
  IntMap (MenuItem a)

simpleMenuItem :: a -> Text -> MenuItem a
simpleMenuItem a t =
  MenuItem a t t

intItems :: [Int] -> Items Int
intItems nums =
  IntMap.fromList [(i, simpleMenuItem i (show i)) | i <- nums]
