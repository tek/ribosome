module Ribosome.Menu.Data.MenuConsumerAction where

import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

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
  |
  UpdatePrompt Prompt
  deriving (Functor)
