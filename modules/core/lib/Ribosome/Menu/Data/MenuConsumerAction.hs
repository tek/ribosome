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
  Render Bool
  |
  Return a
  deriving (Functor)
