module Ribosome.Menu.Data.WindowConfig where

import Ribosome.Data.Mapping (MappingSpec)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Menu.Prompt.Data.Prompt (PromptState)

data WindowOptions =
  WindowOptions {
    prompt :: PromptState,
    items :: ScratchOptions,
    status :: Maybe ScratchOptions,
    builtinHandlers :: Bool,
    defaultHandlers :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Default WindowOptions where
  def =
    WindowOptions {
      prompt = def,
      items = def,
      status = Just def,
      builtinHandlers = True,
      defaultHandlers = True
    }

data WindowConfig =
  WindowConfig {
    items :: ScratchOptions,
    status :: Maybe ScratchOptions,
    mappings :: [MappingSpec]
  }
  deriving stock (Eq, Show, Generic)

instance Default WindowConfig where
  def =
    WindowConfig {
      items = def,
      status = Just def,
      mappings = mempty
    }

toWindowConfig ::
  WindowOptions ->
  [MappingSpec] ->
  WindowConfig
toWindowConfig WindowOptions {..} mappings =
  WindowConfig {..}
