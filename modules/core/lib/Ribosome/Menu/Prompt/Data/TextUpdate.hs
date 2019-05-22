module Ribosome.Menu.Prompt.Data.TextUpdate where

data TextUpdate =
  Unmodified
  |
  Insert Text
  |
  DeleteLeft
  |
  DeleteRight
  deriving (Eq, Show)
