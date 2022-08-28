-- |Path combinators.
module Ribosome.Host.Path where

import Path (Path, toFilePath)

-- |Render a 'Path' as 'Text'.
pathText ::
  Path b t ->
  Text
pathText =
  toText . toFilePath
