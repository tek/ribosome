module Ribosome.Menu.Effect.PromptControl where

import Ribosome.Menu.Prompt.Data.PromptControlEvent (PromptControlEvent)

data PromptControl :: Effect where
  QuitPrompt :: PromptControl m ()
  WaitPromptQuit :: PromptControl m ()
  StartPrompt :: PromptControl m ()
  WaitPromptListening :: PromptControl m ()
  ControlEvent :: PromptControl m (Maybe PromptControlEvent)
  SendControlEvent :: PromptControlEvent -> PromptControl m ()

makeSem ''PromptControl
