module Ribosome.Menu.Prompt.Data.Prompt where

import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode)

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
     state :: PromptMode,
     text :: PromptText
  }
  deriving stock (Eq, Show, Generic)

instance Default Prompt where
  def =
    Prompt 0 PromptMode.Normal ""
