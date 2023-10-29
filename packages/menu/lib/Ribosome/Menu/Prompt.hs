module Ribosome.Menu.Prompt (
  module Ribosome.Menu.Prompt.Data.Prompt,
  module Ribosome.Menu.Prompt.Data.PromptMode,
  module Ribosome.Menu.Prompt.Data.PromptEvent,
  menuPrompt,
  menuPromptState,
) where

import Ribosome.Menu.Action (MenuSem)
import Ribosome.Menu.Prompt.Data.Prompt
import Ribosome.Menu.Prompt.Data.PromptEvent
import Ribosome.Menu.Prompt.Data.PromptMode

menuPromptState ::
  MenuSem s r PromptState
menuPromptState = ask

menuPrompt ::
  MenuSem s r Prompt
menuPrompt = asks (.prompt)
