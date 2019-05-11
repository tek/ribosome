module Ribosome.Menu.Data.PromptConfig where

import Conduit (ConduitT)

import Ribosome.Menu.Data.Prompt (Prompt)
import Ribosome.Menu.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Data.PromptState (PromptState)
import Ribosome.Menu.Data.PromptUpdate (PromptUpdate)

data PromptConfig m =
  PromptConfig {
    _source :: ConduitT () PromptEvent m (),
    _modes :: PromptEvent -> PromptState -> m PromptUpdate,
    _render :: Prompt -> m ()
  }
