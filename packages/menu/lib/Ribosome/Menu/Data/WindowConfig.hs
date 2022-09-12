module Ribosome.Menu.Data.WindowConfig where

import qualified Data.Map.Strict as Map

import Ribosome.Data.Mapping (MappingSpec)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)

data WindowOptions =
  WindowOptions {
    prompt :: PromptConfig,
    items :: ScratchOptions,
    status :: Maybe ScratchOptions
  }
  deriving stock (Eq, Show, Generic)

instance Default WindowOptions where
  def =
    WindowOptions {
      prompt = def,
      items = def,
      status = Just def
    }

data WindowConfig =
  WindowConfig {
    prompt :: PromptConfig,
    items :: ScratchOptions,
    status :: Maybe ScratchOptions,
    mappings :: [MappingSpec]
  }
  deriving stock (Eq, Show, Generic)

instance Default WindowConfig where
  def =
    WindowConfig {
      prompt = def,
      items = def,
      status = Just def,
      mappings = mempty
    }

toWindowConfig ::
  WindowOptions ->
  Map MappingSpec a ->
  WindowConfig
toWindowConfig WindowOptions {..} maps =
  WindowConfig {mappings = Map.keys maps, ..}
