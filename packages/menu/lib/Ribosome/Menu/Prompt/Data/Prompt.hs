module Ribosome.Menu.Prompt.Data.Prompt where

import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState
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

newtype PromptText =
  PromptText { unPromptText :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)

data Prompt =
  Prompt {
     cursor :: Int,
     state :: PromptState,
     text :: PromptText
  }
  deriving stock (Eq, Show, Generic)

instance Default Prompt where
  def =
    Prompt 0 PromptState.Normal ""
