module Ribosome.Data.FileBuffer where

import Path (Abs, File, Path)

import Ribosome.Host.Api.Data (Buffer)

data FileBuffer =
  FileBuffer {
    buffer :: Buffer,
    path :: Path Abs File
  }
  deriving stock (Eq, Show)
