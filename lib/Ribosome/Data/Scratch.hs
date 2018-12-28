module Ribosome.Data.Scratch(
  Scratch(..),
) where

import Neovim (Buffer, Window, Tabpage)

data Scratch =
  Scratch {
    scratchBuffer :: Buffer,
    scratchWindow :: Window,
    scratchTab :: Maybe Tabpage
  }
  deriving (Eq, Show)
