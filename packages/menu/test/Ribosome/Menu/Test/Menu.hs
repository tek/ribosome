module Ribosome.Menu.Test.Menu where

import Conc (Consume, Restoration, consumeFind, interpretQueueTBM, resultToMaybe)
import Exon (exon)
import Polysemy.Chronos (ChronosTime, interpretTimeChronos)
import Polysemy.Test (Hedgehog, TestError, assertEq, evalMaybe)
import qualified Queue
import Time (Seconds (Seconds))

import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Menu.Action (MenuWidget, menuIgnore)
import Ribosome.Menu.Combinators (sortedEntries)
import qualified Ribosome.Menu.Data.Entry as Entry
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Rendered))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Effect.MenuTest (waitEvent)
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Effect.MenuUi (MenuUi)
import Ribosome.Menu.Interpreter.MenuTest (TestTimeout, failTimeout)
import Ribosome.Menu.Interpreter.MenuUi (interpretMenuUiNull)
import Ribosome.Menu.MenuTest (MenuRenderEffects, MenuTestIOStack, MenuTestStack, runTestMenu, testMenuRender)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (startInsert)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Test.Error (testError)

enqueueItems ::
  Members [MenuUi, Hedgehog IO, Queue [Text]] r =>
  Sem r a ->
  Sem r a
enqueueItems =
  intercept \case
    MenuUi.Render menu ->
      evalMaybe . resultToMaybe =<< Queue.writeTimeout (Seconds 5) (MenuItem.text . Entry.item <$> menu ^. #items . sortedEntries)
    MenuUi.RenderPrompt _ ->
      unit
    MenuUi.PromptEvent _ ->
      pure PromptEvent.Ignore

enqueuePrompt ::
  ∀ i r a .
  Members [Hedgehog IO, Queue Prompt, Resource, Embed IO] r =>
  MenuWidget i r a
enqueuePrompt = do
  Queue.write =<< ask
  menuIgnore

type SimpleTestMenu =
  [
    MenuUi,
    ChronosTime,
    Queue [Text],
    Queue Prompt
  ]

runSimpleTestMenu ::
  Members [Reader TestTimeout, Fail, Hedgehog IO, Log, Resource, Race, Mask Restoration, Embed IO, Final IO] r =>
  InterpretersFor SimpleTestMenu r
runSimpleTestMenu =
  interpretQueueTBM @Prompt 64 .
  interpretQueueTBM @[Text] 64 .
  interpretTimeChronos .
  interpretMenuUiNull .
  enqueueItems

type PromptTest i =
  Consume MenuEvent : MenuRenderEffects i () ++ Stop RpcError : SimpleTestMenu ++ MenuTestStack i ()

promptTest ::
  Show i =>
  Members MenuTestIOStack r =>
  Members [Hedgehog IO, Error TestError, Fail, Log, Resource, Race, Mask Restoration, Async, Embed IO, Final IO] r =>
  InterpretersFor (PromptTest i) r
promptTest sem =
  runTestMenu startInsert $
  runSimpleTestMenu $
  testError $
  testMenuRender startInsert (const (Just enqueuePrompt)) $
  subscribe @MenuEvent do
    waitEvent "initial render" Rendered
    assertItems []
    sem

assertPrompt ::
  HasCallStack =>
  Members [Hedgehog IO, Reader TestTimeout, Fail, Consume MenuEvent, Race] r =>
  Text ->
  Prompt ->
  Sem r ()
assertPrompt desc p =
  withFrozenCallStack do
    void $ failTimeout [exon|prompt #{desc}|] $ consumeFind \case
      MenuEvent.PromptUpdated newP | newP == p -> pure True
      _ -> pure False

currentItems ::
  HasCallStack =>
  Members [Queue [Text], Hedgehog IO] r =>
  Sem r [Text]
currentItems =
  withFrozenCallStack do
    evalMaybe . resultToMaybe =<< Queue.readTimeout (Seconds 5)

assertItems ::
  HasCallStack =>
  Members [Hedgehog IO, Queue [Text]] r =>
  [Text] ->
  Sem r ()
assertItems i =
  withFrozenCallStack do
    assertEq i =<< currentItems
