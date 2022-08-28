-- |Path combinators.
module Ribosome.Path where

import Path (Path, toFilePath)

-- |Render a 'Path' as 'Text'.
pathText ::
  Path b t ->
  Text
pathText =
  toText . toFilePath
