module Ribosome.Menu.Prompt.Data.PromptRenderer where

import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data PromptRenderer m =
  ∀ a. PromptRenderer {
    acquire :: m a,
    release :: a -> m (),
    render :: Prompt -> m ()
  }
