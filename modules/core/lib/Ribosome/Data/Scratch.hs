module Ribosome.Data.Scratch(
  Scratch(..),
) where

import Ribosome.Nvim.Api.Data (Buffer, Tabpage, Window)

data Scratch =
  Scratch {
    scratchBuffer :: Buffer,
    scratchWindow :: Window,
    scratchTab :: Maybe Tabpage
  }
  deriving (Eq, Show)
