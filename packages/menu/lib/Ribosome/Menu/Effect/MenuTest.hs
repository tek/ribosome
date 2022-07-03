module Ribosome.Menu.Effect.MenuTest where

import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent
import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)

data MenuTest i a :: Effect where
  SendItem :: MenuItem i -> MenuTest i a m ()
  ItemsDone :: MenuTest i a m ()
  SendPrompt :: Bool -> PromptInputEvent -> MenuTest i a m ()
  Result :: MenuTest i a m (MenuResult a)

makeSem ''MenuTest

sendSimpleItem ::
  Member (MenuTest i a) r =>
  i ->
  Text ->
  Sem r ()
sendSimpleItem i t =
  sendItem (simpleMenuItem i t)

sendStaticItems ::
  Member (MenuTest i a) r =>
  [MenuItem i] ->
  Sem r ()
sendStaticItems items = do
  traverse_ sendItem items
  itemsDone

sendCharWait ::
  Member (MenuTest i a) r =>
  Text ->
  Sem r ()
sendCharWait c =
  sendPrompt True (PromptInputEvent.Character c)

sendChar ::
  Member (MenuTest i a) r =>
  Text ->
  Sem r ()
sendChar c =
  sendPrompt False (PromptInputEvent.Character c)
