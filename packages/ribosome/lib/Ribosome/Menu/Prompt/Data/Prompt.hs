module Ribosome.Menu.Prompt.Data.Prompt where

import Prelude hiding (state)

import Ribosome.Menu.Prompt.Data.PromptState (PromptState)

data PromptChange =
  PromptAppend
  |
  PromptUnappend
  |
  PromptRandom
  deriving stock (Eq, Show)

instance Default PromptChange where
  def =
    PromptRandom

data Prompt =
  Prompt {
     _cursor :: Int,
     _state :: PromptState,
     _text :: Text,
     _lastChange :: PromptChange
  }
  deriving stock (Eq, Show)

deepLenses ''Prompt
