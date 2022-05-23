module Ribosome.Menu.Prompt.Data.CursorUpdate where

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
  deriving stock (Eq, Show)
