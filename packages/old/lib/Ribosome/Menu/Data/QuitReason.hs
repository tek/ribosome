module Ribosome.Menu.Data.QuitReason where

data QuitReason =
  Aborted
  |
  Error Text
  deriving stock (Eq, Show)
