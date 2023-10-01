module Ribosome.Menu.Effect.MenuTest where

import qualified Data.Text as Text
import Exon (exon)

import Ribosome.Data.Mapping (MappingLhs)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Rendered))
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

sendPromptRender ::
  Member (MenuTest i a) r =>
  Prompt ->
  Sem r ()
sendPromptRender p = do
  sendPrompt p
  waitEvent [exon|Render for prompt #{show p}|] Rendered

setPromptWait ::
  Member (MenuTest i a) r =>
  Text ->
  Sem r ()
setPromptWait t =
  sendPromptWait (fromText t)

setPrompt ::
  Member (MenuTest i a) r =>
  Text ->
  Sem r ()
setPrompt t =
  sendPrompt (fromText t)

sendMappingQuery ::
  Member (MenuTest i a) r =>
  MappingLhs ->
  Sem r ()
sendMappingQuery m =
  sendPromptEvent True (PromptEvent.Mapping m)

sendMappingPrompt ::
  Member (MenuTest i a) r =>
  MappingLhs ->
  Sem r ()
sendMappingPrompt m = do
  sendPromptEvent False (PromptEvent.Mapping m)
  waitEventsPred [exon|Prompt update for mapping ##{m}|] $ pure \case
    MenuEvent.PromptUpdated _ -> True
    _ -> False

sendMapping ::
  Member (MenuTest i a) r =>
  MappingLhs ->
  Sem r ()
sendMapping m =
  sendPromptEvent False (PromptEvent.Mapping m)

sendMappingRender ::
  Member (MenuTest i a) r =>
  MappingLhs ->
  Sem r ()
sendMappingRender m = do
  sendMapping m
  waitEvent [exon|Render for mapping ##{m}|] Rendered
