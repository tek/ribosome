module Ribosome.Menu.Prompt.Data.Prompt where

import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode, PromptMode(Insert))

data PromptModes =
  StartNormal
  |
  StartInsert
  |
  OnlyInsert
  deriving stock (Eq, Show)

instance Default PromptModes where
  def = StartNormal

shouldStartInsert :: PromptModes -> Bool
shouldStartInsert = \case
  StartNormal -> False
  StartInsert -> True
  OnlyInsert -> True

startInsert :: PromptModes
startInsert = StartInsert

onlyInsert :: PromptModes
onlyInsert = OnlyInsert

data PromptChange =
  Append
  |
  Random
  deriving stock (Eq, Show)

newtype CursorCol =
  CursorCol Int
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

newtype PromptText =
  PromptText { unPromptText :: Text }
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (IsString)

data Prompt =
  Prompt {
     cursor :: CursorCol,
     mode :: PromptMode,
     text :: PromptText
  }
  deriving stock (Eq, Show, Ord, Generic)

instance Default Prompt where
  def = Prompt 0 PromptMode.Normal ""

instance IsString Prompt where
  fromString s = Prompt (CursorCol (length s)) PromptMode.Normal (fromString s)

data PromptControl =
  PromptControlApp
  |
  PromptControlItems
  deriving stock (Eq, Show, Ord)

instance Default PromptControl where
  def = PromptControlItems

data PromptState =
  PromptState {
     prompt :: Prompt,
     control :: PromptControl,
     modes :: PromptModes
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

initPrompt :: PromptState -> PromptState
initPrompt s@PromptState {..}
  | shouldStartInsert modes = PromptState {prompt = prompt & #mode .~ Insert, ..}
  | otherwise = s
