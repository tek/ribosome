module Ribosome.Menu.Effect.MenuUi where

import Conc (PScoped, pscoped)

import Ribosome.Data.Mapping (MappingSpec)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.ScratchState (ScratchState)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

data MenuUi :: Effect where
  RenderPrompt :: Prompt -> MenuUi m ()
  PromptEvent :: Prompt -> MenuUi m PromptEvent
  Render :: Menu i -> MenuUi m ()

makeSem ''MenuUi

withMenuUi ::
  Member (PScoped par res MenuUi) r =>
  par ->
  InterpreterFor MenuUi r
withMenuUi =
  pscoped

data EchoMenu =
  EchoMenu {
    prompt :: PromptConfig,
    scratch :: ScratchState
  }
  deriving stock (Eq, Show)

data WindowMenu =
  WindowMenu {
    scratch :: ScratchState
  }
  deriving stock (Eq, Show)

data PureMenu =
  PureMenu
  deriving stock (Eq, Show)

data NvimMenuConfig =
  NvimMenuConfig {
    prompt :: PromptConfig,
    scratch :: ScratchOptions,
    mappings :: [MappingSpec]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

type NvimMenuUi res =
  PScoped NvimMenuConfig res MenuUi !! RpcError

type EchoMenuUi =
  NvimMenuUi EchoMenu

type WindowMenuUi =
  NvimMenuUi WindowMenu

type PureMenuUi =
  NvimMenuUi PureMenu
