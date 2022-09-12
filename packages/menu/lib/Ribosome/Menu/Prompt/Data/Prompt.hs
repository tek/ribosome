module Ribosome.Menu.Prompt.Data.Prompt where

import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode)

data PromptChange =
  Append
  |
  Unappend
  |
  Random
  deriving stock (Eq, Show)

instance Default PromptChange where
  def =
    Random

newtype PromptText =
  PromptText { unPromptText :: Text }
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (IsString)

data Prompt =
  Prompt {
     cursor :: Int,
     state :: PromptMode,
     text :: PromptText
  }
  deriving stock (Eq, Show, Ord, Generic)

instance Default Prompt where
  def =
    Prompt 0 PromptMode.Normal ""

instance IsString Prompt where
  fromString s =
    Prompt (length s) PromptMode.Normal (fromString s)
