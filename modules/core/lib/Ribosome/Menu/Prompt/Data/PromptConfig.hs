module Ribosome.Menu.Prompt.Data.PromptConfig where

import Conduit (ConduitT)

import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Prompt.Data.PromptState (PromptState)
import Ribosome.Menu.Prompt.Data.PromptUpdate (PromptUpdate)

data PromptConfig m =
  PromptConfig {
    _source :: ConduitT () PromptEvent m (),
    _modes :: PromptEvent -> PromptState -> m PromptUpdate,
    _render :: Prompt -> m ()
  }
