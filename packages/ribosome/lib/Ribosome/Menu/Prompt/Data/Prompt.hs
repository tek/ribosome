module Ribosome.Menu.Prompt.Data.Prompt where

import Prelude hiding (state)

import Ribosome.Menu.Prompt.Data.PromptState (PromptState)

data Prompt =
  Prompt {
     _cursor :: Int,
     _state :: PromptState,
     _text :: Text
  }
  deriving (Eq, Show)

deepLenses ''Prompt
