module Ribosome.Menu.Data.MenuConsumerAction where

data MenuConsumerAction m a =
  Quit
  |
  QuitWith (m a)
  |
  Continue
  |
  Execute (m ())
  |
  Filter
  |
  Render Bool
  |
  Return a
  deriving (Functor)
