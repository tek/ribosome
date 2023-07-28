module Ribosome.Menu.Test.Util where

import qualified Streamly.Prelude as Stream
import Streamly.Prelude (SerialT)

import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)

staticMenuItems ::
  [Text] ->
  [MenuItem Text]
staticMenuItems =
  fmap (simpleMenuItem "name")

mkItems ::
  [Text] ->
  SerialT IO (MenuItem Text)
mkItems =
  Stream.fromList . fmap (simpleMenuItem "name")
