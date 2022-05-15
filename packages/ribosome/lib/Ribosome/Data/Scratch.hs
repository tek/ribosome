module Ribosome.Data.Scratch where

import Ribosome.Host.Api.Data (Buffer, Tabpage, Window)

data Scratch =
  Scratch {
    scratchName :: Text,
    scratchBuffer :: Buffer,
    scratchWindow :: Window,
    scratchPrevious :: Window,
    scratchTab :: Maybe Tabpage
  }
  deriving stock (Eq, Show)
