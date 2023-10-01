module Ribosome.Menu.Data.InputParams where

import Exon (exon)

import Ribosome.Data.Mapping (MapMode, MappingLhs)
import qualified Ribosome.Menu.Prompt.Data.Prompt
import Ribosome.Menu.Prompt.Data.Prompt (
  Prompt (Prompt),
  PromptControl (PromptControlApp, PromptControlItems),
  PromptState (PromptState),
  )
import Ribosome.Menu.Prompt.Data.PromptMode (toMapMode)

data InputTrigger =
  InputMapping MappingLhs
  |
  InputPrompt
  deriving stock (Eq, Show, Ord)

data InputMode =
  InputMode {
    mode :: MapMode,
    prompt :: PromptControl
  }
  deriving stock (Eq, Show, Ord)

describeInputMode :: InputMode -> Text
describeInputMode InputMode {..} =
  [exon|#{show mode} (#{pc})|]
  where
    pc = case prompt of
      PromptControlApp -> "app"
      PromptControlItems -> "items"

promptMode :: PromptState -> InputMode
promptMode PromptState {prompt = Prompt {mode}, control} =
  InputMode {mode = toMapMode mode, prompt = control}

data InputParams =
  InputParams {
    trigger :: InputTrigger,
    mode :: InputMode
  }
  deriving stock (Eq, Show, Ord)

inputPrompt :: PromptState -> InputParams
inputPrompt s =
  InputParams {trigger = InputPrompt, mode = promptMode s}

data InputDomain =
  InputDomain {
    modes :: NonEmpty MapMode,
    prompt :: Maybe PromptControl
  }
  deriving stock (Eq, Show, Ord, Generic)
