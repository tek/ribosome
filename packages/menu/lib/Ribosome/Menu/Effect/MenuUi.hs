module Ribosome.Menu.Effect.MenuUi where

import Conc (PScoped, pscoped)

import Ribosome.Data.ScratchState (ScratchState)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Menu.Data.RenderMenu (RenderMenu)
import Ribosome.Menu.Data.WindowConfig (WindowConfig)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

data MenuUi :: Effect where
  RenderPrompt :: Bool -> Prompt -> MenuUi m ()
  PromptEvent :: Prompt -> MenuUi m PromptEvent
  Render :: RenderMenu i -> MenuUi m ()

makeSem ''MenuUi

withMenuUi ::
  Member (PScoped par res MenuUi) r =>
  par ->
  InterpreterFor MenuUi r
withMenuUi =
  pscoped

data WindowMenu =
  WindowMenu {
    itemsScratch :: ScratchState,
    statusScratch :: Maybe ScratchState,
    promptScratch :: ScratchState
  }
  deriving stock (Eq, Show)

data PureMenu =
  PureMenu
  deriving stock (Eq, Show)

type NvimMenuUi res =
  PScoped WindowConfig res MenuUi !! RpcError

type WindowMenuUi =
  NvimMenuUi WindowMenu

type PureMenuUi =
  NvimMenuUi PureMenu
