module Ribosome.Menu.Prompt.Data.TextUpdate where

data TextUpdate =
  Unmodified
  |
  Insert Text
  |
  DeleteLeft
  |
  DeleteRight
  |
  Set Text
  deriving stock (Eq, Show)
