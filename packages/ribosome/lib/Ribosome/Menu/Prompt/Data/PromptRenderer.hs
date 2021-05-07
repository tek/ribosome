module Ribosome.Menu.Prompt.Data.PromptRenderer where

import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data PromptRenderer m =
  ∀ a. PromptRenderer {
    _acquire :: m a,
    _release :: a -> m (),
    _render :: Prompt -> m ()
  }
