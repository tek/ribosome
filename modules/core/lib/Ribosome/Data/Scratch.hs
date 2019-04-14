module Ribosome.Data.Scratch where

import Ribosome.Nvim.Api.Data (Buffer, Tabpage, Window)

data Scratch =
  Scratch {
    scratchName :: Text,
    scratchBuffer :: Buffer,
    scratchWindow :: Window,
    scratchPrevious :: Window,
    scratchTab :: Maybe Tabpage
  }
  deriving (Eq, Show)
