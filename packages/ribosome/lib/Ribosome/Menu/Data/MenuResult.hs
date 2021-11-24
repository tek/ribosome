module Ribosome.Menu.Data.MenuResult where

data MenuResult a =
  NoOutput
  |
  Error Text
  |
  Executed
  |
  Aborted
  |
  Return a
  deriving stock (Eq, Show)
