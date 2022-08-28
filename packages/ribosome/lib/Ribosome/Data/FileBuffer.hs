-- |Data type representing a buffer associated with a file system path.
module Ribosome.Data.FileBuffer where

import Path (Abs, File, Path)

import Ribosome.Host.Api.Data (Buffer)

-- |Data type representing a buffer associated with a file system path.
data FileBuffer =
  FileBuffer {
    buffer :: Buffer,
    path :: Path Abs File
  }
  deriving stock (Eq, Show)
