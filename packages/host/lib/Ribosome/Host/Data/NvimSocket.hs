module Ribosome.Host.Data.NvimSocket where

import Path (Abs, File, Path)

newtype NvimSocket =
  NvimSocket { unNvimSocket :: Path Abs File }
  deriving stock (Eq, Show)
