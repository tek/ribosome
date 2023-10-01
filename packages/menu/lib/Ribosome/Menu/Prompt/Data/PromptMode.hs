module Ribosome.Menu.Prompt.Data.PromptMode where

import Ribosome.Data.Mapping (MapMode (MapInsert, MapNormal))

data PromptMode =
  Insert
  |
  Normal
  deriving stock (Eq, Show, Ord)

toMapMode :: PromptMode -> MapMode
toMapMode = \case
  Insert -> MapInsert
  Normal -> MapNormal
