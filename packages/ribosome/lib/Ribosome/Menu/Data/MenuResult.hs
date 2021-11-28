module Ribosome.Menu.Data.MenuResult where

data MenuResult a =
  Success a
  |
  Aborted
  |
  Error Text
  deriving stock (Eq, Show, Functor)
