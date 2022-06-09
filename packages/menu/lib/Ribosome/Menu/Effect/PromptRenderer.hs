module Ribosome.Menu.Effect.PromptRenderer where

import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Nvim (NvimPromptResources)

data PromptRenderer :: Effect where
  RenderPrompt :: Prompt -> PromptRenderer m ()

makeSem ''PromptRenderer

withPrompt ::
  Member (Scoped res PromptRenderer) r =>
  InterpreterFor PromptRenderer r
withPrompt =
  scoped

type NvimPrompt =
  Scoped NvimPromptResources PromptRenderer
