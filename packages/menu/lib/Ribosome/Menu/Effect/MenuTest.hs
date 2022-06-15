module Ribosome.Menu.Effect.MenuTest where

import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent
import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)

data MenuTest i :: Effect where
  SendItem :: MenuItem i -> MenuTest i m ()
  ItemsDone :: MenuTest i m ()
  SendPrompt :: PromptInputEvent -> MenuTest i m ()

makeSem ''MenuTest

sendSimpleItem ::
  Member (MenuTest i) r =>
  i ->
  Text ->
  Sem r ()
sendSimpleItem i t =
  sendItem (simpleMenuItem i t)

sendStaticItems ::
  Member (MenuTest i) r =>
  [MenuItem i] ->
  Sem r ()
sendStaticItems items = do
  traverse_ sendItem items
  itemsDone

sendChar ::
  Member (MenuTest i) r =>
  Text ->
  Sem r ()
sendChar c =
  sendPrompt (PromptInputEvent.Character c)
