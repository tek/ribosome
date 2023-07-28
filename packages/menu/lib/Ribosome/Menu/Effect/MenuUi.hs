module Ribosome.Menu.Effect.MenuUi where

import Ribosome.Data.ScratchState (ScratchState)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Menu.Data.RenderMenu (RenderMenu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

data MenuUi :: Effect where
  RenderPrompt :: Bool -> Prompt -> MenuUi m ()
  PromptEvent :: MenuUi m PromptEvent
  Render :: RenderMenu i -> MenuUi m ()
  PromptScratch :: MenuUi m ScratchState
  StatusScratch :: MenuUi m ScratchState
  ItemsScratch :: MenuUi m ScratchState

makeSem ''MenuUi

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

type ScopedMenuUi param =
  Scoped param (MenuUi !! RpcError) !! RpcError
