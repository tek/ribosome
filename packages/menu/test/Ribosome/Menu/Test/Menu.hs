module Ribosome.Menu.Test.Menu where

import Conc (Consume, Restoration, consumeFind, interpretQueueTBM, resultToMaybe)
import Exon (exon)
import Hedgehog.Internal.Property (Failure)
import Polysemy.Chronos (ChronosTime, interpretTimeChronos)
import Polysemy.Test (Hedgehog, TestError, assertEq, evalMaybe)
import qualified Queue
import Time (Seconds (Seconds))

import Ribosome.Host.Data.Report (ReportLog)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Interpreter.Log (interpretReportLogLog)
import Ribosome.Menu.Action (MenuWidget, menuIgnore)
import Ribosome.Menu.Class.MenuState (MenuState)
import Ribosome.Menu.Combinators (sortEntries, sortedEntries)
import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Filter (Filter (Fuzzy))
import Ribosome.Menu.Data.FilterMode (FilterMode)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.State (Modal, modal)
import Ribosome.Menu.Effect.Menu (Menu, readState)
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import qualified Ribosome.Menu.Effect.MenuTest as MenuTest
import Ribosome.Menu.Effect.MenuTest (MenuTest)
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Effect.MenuUi (MenuUi)
import Ribosome.Menu.Interpreter.MenuFilter (defaultFilter)
import Ribosome.Menu.Interpreter.MenuTest (TestTimeout, failTimeout)
import Ribosome.Menu.Interpreter.MenuUi (interpretMenuUiNull)
import Ribosome.Menu.MenuTest (MenuRenderEffects, MenuTestIOStack, MenuTestStack, runTestMenu, testMenuRender)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), PromptText)
import Ribosome.Menu.Prompt.Data.PromptConfig (startInsert)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Test.Error (testError)
import Ribosome.Test.Wait (assertWait)

enqueueItems ::
  Members [MenuUi !! RpcError, Hedgehog IO, Queue [Text]] r =>
  Sem r a ->
  Sem r a
enqueueItems =
  interceptResumable \case
    MenuUi.Render menu ->
      evalMaybe . resultToMaybe =<< Queue.writeTimeout (Seconds 5) (MenuItem.text . Entry.item <$> menu ^. #entries . to sortEntries)
    MenuUi.RenderPrompt _ _ ->
      unit
    MenuUi.PromptEvent _ ->
      pure PromptEvent.Ignore

enqueuePrompt ::
  ∀ s r a .
  Members [Hedgehog IO, Queue Prompt, Resource, Embed IO] r =>
  MenuWidget s r a
enqueuePrompt = do
  Queue.write =<< ask
  menuIgnore

type SimpleTestMenu =
  [
    MenuUi !! RpcError,
    ReportLog,
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
  interpretReportLogLog .
  raiseResumable interpretMenuUiNull .
  enqueueItems

type PromptTest i =
  Consume MenuEvent : MenuRenderEffects (Modal Filter i) () ++ MenuFilter (FilterMode Filter) : Stop RpcError : SimpleTestMenu ++ MenuTestStack i ()

promptTest ::
  ∀ i r .
  Show i =>
  Members MenuTestIOStack r =>
  Members [Hedgehog IO, Error TestError, Fail, Log, Resource, Race, Mask Restoration, Async, Embed IO, Final IO] r =>
  InterpretersFor (PromptTest i) r
promptTest sem =
  runTestMenu @_ @i startInsert $
  runSimpleTestMenu $
  testError $
  defaultFilter $
  testMenuRender @_ @(Modal Filter i) startInsert (modal Fuzzy) (const (Just enqueuePrompt)) $
  subscribe @MenuEvent do
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

assertPromptText ::
  HasCallStack =>
  Members [Hedgehog IO, Reader TestTimeout, Fail, Consume MenuEvent, Race] r =>
  Text ->
  PromptText ->
  Sem r ()
assertPromptText desc p =
  withFrozenCallStack do
    void $ failTimeout [exon|prompt #{desc}|] $ consumeFind \case
      MenuEvent.PromptUpdated (Prompt _ _ newP) | newP == p -> pure True
      _ -> pure False

setPrompt ::
  HasCallStack =>
  Members [MenuTest i a, Hedgehog IO, Reader TestTimeout, Fail, Consume MenuEvent, Race] r =>
  Text ->
  PromptText ->
  Sem r ()
setPrompt desc p =
  withFrozenCallStack do
    MenuTest.setPrompt p
    assertPromptText desc p

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

currentEntries ::
  MenuState s =>
  Members [Hedgehog IO, Menu s] r =>
  Sem r [Text]
currentEntries =
  toListOf (sortedEntries . each . #item . #text) <$> readState

awaitCurrent ::
  MenuState s =>
  HasCallStack =>
  Members [Menu s, Hedgehog IO, ChronosTime, Error Failure, Race, Async, Embed IO] r =>
  [Text] ->
  Sem r ()
awaitCurrent target =
  withFrozenCallStack do
    assertWait currentEntries (assertEq target)
