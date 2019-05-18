module Ribosome.Menu.Data.MenuConsumerAction where

data MenuConsumerAction m =
  Quit
  |
  QuitWith (m ())
  |
  Continue
