module Ribosome.Menu.Effect.MenuTest where

import qualified Data.Text as Text
import Exon (exon)
import Streamly.Prelude (SerialT)

import Ribosome.Data.Mapping (MappingLhs)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Rendered))
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Data.TestMenuConfig (TestMenuConfig, TestTimeout)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

data MenuTest i result :: Effect where
  Config :: MenuTest i result m (TestMenuConfig i)
  ItemsStream :: MenuTest i result m (SerialT IO (MenuItem i))
  PromptEvent :: MenuTest i result m PromptEvent
  SendItem :: MenuItem i -> MenuTest i result m ()
  ItemsDone :: Text -> MenuTest i result m ()
  WaitItemsDone :: Text -> MenuTest i result m ()
  SendPromptEvent :: Bool -> PromptEvent -> MenuTest i result m ()
  WaitEventsPred :: Text -> [MenuEvent -> Bool] -> MenuTest i result m ()
  Result :: MenuTest i result m (MenuResult result)
  NextEvent :: MenuTest i result m MenuEvent
  LoopFinished :: MenuResult result -> MenuTest i result m Bool
  Timeout :: MenuTest i result m TestTimeout
  Quit :: MenuTest i result m ()

makeSem ''MenuTest

type MenuTests i result =
  Scoped (TestMenuConfig i) (MenuTest i result)

sendSimpleItem ::
  Member (MenuTest i result) r =>
  i ->
  Text ->
  Sem r ()
sendSimpleItem i t =
  sendItem (simpleMenuItem i t)

waitEventPred ::
  Member (MenuTest i result) r =>
  Text ->
  (MenuEvent -> Bool) ->
  Sem r ()
waitEventPred desc ev =
  waitEventsPred desc [ev]

waitEvents ::
  Member (MenuTest i result) r =>
  Text ->
  [MenuEvent] ->
  Sem r ()
waitEvents desc evs =
  waitEventsPred [exon|#{desc} (#{Text.intercalate ", " (show <$> evs)})|] ((==) <$> evs)

waitEvent ::
  Member (MenuTest i result) r =>
  Text ->
  MenuEvent ->
  Sem r ()
waitEvent desc ev =
  waitEvents desc [ev]

sendStaticItems ::
  Member (MenuTest i result) r =>
  Text ->
  [MenuItem i] ->
  Sem r ()
sendStaticItems desc items = do
  traverse_ sendItem items
  itemsDone desc

sendPromptEvents ::
  Member (MenuTest i result) r =>
  Bool ->
  [PromptEvent] ->
  Sem r ()
sendPromptEvents wait =
  traverse_ (sendPromptEvent wait)

sendPromptWait ::
  Member (MenuTest i result) r =>
  Prompt ->
  Sem r ()
sendPromptWait p =
  sendPromptEvent True (PromptEvent.Update p)

sendPrompt ::
  Member (MenuTest i result) r =>
  Prompt ->
  Sem r ()
sendPrompt p =
  sendPromptEvent False (PromptEvent.Update p)

sendPromptRender ::
  Member (MenuTest i result) r =>
  Prompt ->
  Sem r ()
sendPromptRender p = do
  sendPrompt p
  waitEvent [exon|Render for prompt #{show p}|] Rendered

setPromptWait ::
  Member (MenuTest i result) r =>
  Text ->
  Sem r ()
setPromptWait t =
  sendPromptWait (fromText t)

setPrompt ::
  Member (MenuTest i result) r =>
  Text ->
  Sem r ()
setPrompt t =
  sendPrompt (fromText t)

sendMappingQuery ::
  Member (MenuTest i result) r =>
  MappingLhs ->
  Sem r ()
sendMappingQuery m =
  sendPromptEvent True (PromptEvent.Mapping m)

sendMappingPrompt ::
  Member (MenuTest i result) r =>
  MappingLhs ->
  Sem r ()
sendMappingPrompt m = do
  sendPromptEvent False (PromptEvent.Mapping m)
  waitEventsPred [exon|Prompt update for mapping ##{m}|] $ pure \case
    MenuEvent.PromptUpdated _ -> True
    _ -> False

sendMapping ::
  Member (MenuTest i result) r =>
  MappingLhs ->
  Sem r ()
sendMapping m =
  sendPromptEvent False (PromptEvent.Mapping m)

sendMappingRender ::
  Member (MenuTest i result) r =>
  MappingLhs ->
  Sem r ()
sendMappingRender m = do
  sendMapping m
  waitEvent [exon|Render for mapping ##{m}|] Rendered
