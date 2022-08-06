module Ribosome.Menu.Effect.MenuTest where

import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

data MenuTest i a :: Effect where
  SendItem :: MenuItem i -> MenuTest i a m ()
  ItemsDone :: Text -> MenuTest i a m ()
  WaitItemsDone :: Text -> MenuTest i a m ()
  SendPromptEvent :: Bool -> PromptEvent -> MenuTest i a m ()
  SendCharEvent :: Bool -> Text -> MenuTest i a m ()
  WaitEventPred :: Text -> (MenuEvent -> Bool) -> MenuTest i a m ()
  WaitEvent :: Text -> MenuEvent -> MenuTest i a m ()
  Result :: MenuTest i a m (MenuResult a)
  NextEvent :: MenuTest i a m MenuEvent

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
  Text ->
  [MenuItem i] ->
  Sem r ()
sendStaticItems desc items = do
  traverse_ sendItem items
  itemsDone desc

sendPromptWait ::
  Member (MenuTest i a) r =>
  Prompt ->
  Sem r ()
sendPromptWait p =
  sendPromptEvent True (PromptEvent.Update p)

sendPrompt ::
  Member (MenuTest i a) r =>
  Prompt ->
  Sem r ()
sendPrompt p =
  sendPromptEvent False (PromptEvent.Update p)

sendCharWait ::
  Member (MenuTest i a) r =>
  Text ->
  Sem r ()
sendCharWait =
  sendCharEvent True

sendChar ::
  Member (MenuTest i a) r =>
  Text ->
  Sem r ()
sendChar =
  sendCharEvent False
