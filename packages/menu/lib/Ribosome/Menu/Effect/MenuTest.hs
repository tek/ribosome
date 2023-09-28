module Ribosome.Menu.Effect.MenuTest where

import qualified Data.Text as Text
import Exon (exon)

import Ribosome.Menu.Data.MenuEvent (MenuEvent (Rendered))
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), PromptText (PromptText))
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode (Normal))

data MenuTest i a :: Effect where
  SendItem :: MenuItem i -> MenuTest i a m ()
  ItemsDone :: Text -> MenuTest i a m ()
  WaitItemsDone :: Text -> MenuTest i a m ()
  SendPromptEvent :: Bool -> PromptEvent -> MenuTest i a m ()
  WaitEventsPred :: Text -> [MenuEvent -> Bool] -> MenuTest i a m ()
  Result :: MenuTest i a m (MenuResult a)
  NextEvent :: MenuTest i a m MenuEvent
  Quit :: MenuTest i a m ()

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

waitEventPred ::
  Member (MenuTest i a) r =>
  Text ->
  (MenuEvent -> Bool) ->
  Sem r ()
waitEventPred desc ev =
  waitEventsPred desc [ev]

waitEvents ::
  Member (MenuTest i a) r =>
  Text ->
  [MenuEvent] ->
  Sem r ()
waitEvents desc evs =
  waitEventsPred [exon|#{desc} (#{Text.intercalate ", " (show <$> evs)})|] ((==) <$> evs)

waitEvent ::
  Member (MenuTest i a) r =>
  Text ->
  MenuEvent ->
  Sem r ()
waitEvent desc ev =
  waitEvents desc [ev]

sendPromptEvents ::
  Member (MenuTest i a) r =>
  Bool ->
  [PromptEvent] ->
  Sem r ()
sendPromptEvents wait =
  traverse_ (sendPromptEvent wait)

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

setPromptWait ::
  Member (MenuTest i a) r =>
  PromptText ->
  Sem r ()
setPromptWait t =
  sendPromptWait (Prompt (Text.length (coerce t)) Normal t)

setPrompt ::
  Member (MenuTest i a) r =>
  PromptText ->
  Sem r ()
setPrompt t =
  sendPrompt (Prompt (Text.length (coerce t)) Normal t)

sendMappingPrompt ::
  Member (MenuTest i a) r =>
  Text ->
  Sem r ()
sendMappingPrompt m =
  sendPromptEvent True (PromptEvent.Mapping m)

sendMapping ::
  Member (MenuTest i a) r =>
  Text ->
  Sem r ()
sendMapping m =
  sendPromptEvent False (PromptEvent.Mapping m)

sendMappingRender ::
  Member (MenuTest i a) r =>
  Text ->
  Sem r ()
sendMappingRender m = do
  sendMapping m
  waitEvent [exon|Render for mapping #{m}|] Rendered
