module Ribosome.Menu.Data.MenuEvent where

data MenuEvent =
  Character Text
  |
  Quit
  deriving (Eq, Show)
