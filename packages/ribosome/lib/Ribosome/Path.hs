module Ribosome.Path where

import Path (Path, toFilePath)

pathText ::
  Path b t ->
  Text
pathText =
  toText . toFilePath
