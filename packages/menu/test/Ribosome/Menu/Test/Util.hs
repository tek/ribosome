module Ribosome.Menu.Test.Util where

import qualified Streamly.Data.Stream.Prelude as Stream
import Streamly.Data.Stream.Prelude (Stream)

import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)

staticMenuItems ::
  [Text] ->
  [MenuItem Text]
staticMenuItems =
  fmap \ t -> simpleMenuItem t t

mkItems ::
  [Text] ->
  Stream IO (MenuItem Text)
mkItems =
  Stream.fromList . fmap (simpleMenuItem "name")
