module Ribosome.Menu.Data.CursorUpdate where

data CursorUpdate =
  Unmodified
  |
  OneLeft
  |
  OneRight
  |
  Append
  |
  Prepend
  |
  Index Int
  deriving (Eq, Show)
