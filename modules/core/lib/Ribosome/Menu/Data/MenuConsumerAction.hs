module Ribosome.Menu.Data.MenuConsumerAction where

data MenuConsumerAction m a =
  Quit
  |
  QuitWith (m a)
  |
  Continue
  |
  Render Bool
  |
  Return a
  deriving (Functor)
