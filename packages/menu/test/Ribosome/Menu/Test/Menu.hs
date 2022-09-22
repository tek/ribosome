module Ribosome.Menu.Test.Menu where

import Conc (Consume, Restoration, consumeFind, interpretQueueTBM, interpretScopedR_, resultToMaybe)
import Exon (exon)
import Hedgehog.Internal.Property (Failure)
import Polysemy.Chronos (ChronosTime, interpretTimeChronos)
import Polysemy.Test (Hedgehog, TestError, assertEq, evalMaybe)
import qualified Queue
import Time (Seconds (Seconds))

import Ribosome.Host.Data.Report (ReportLog, resumeReportFail)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Interpreter.Log (interpretReportLogLog)
import Ribosome.Menu.Action (MenuWidget, menuIgnore)
import Ribosome.Menu.Class.MenuState (MenuState)
import Ribosome.Menu.Combinators (sortEntries)
import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Filter (Filter (Fuzzy))
import Ribosome.Menu.Data.FilterMode (FilterMode)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.State (Modal, modal)
import Ribosome.Menu.Effect.Menu (Menu, Menus, UiMenus, bundleMenuEngine)
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import qualified Ribosome.Menu.Effect.MenuTest as MenuTest
import Ribosome.Menu.Effect.MenuTest (MenuTest)
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Effect.MenuUi (ScopedMenuUi)
import Ribosome.Menu.Interpreter.Menu (interpretMenus)
import Ribosome.Menu.Interpreter.MenuFilter (defaultFilter)
import Ribosome.Menu.Interpreter.MenuTest (TestTimeout, failTimeout)
import Ribosome.Menu.Items (currentEntries)
import Ribosome.Menu.Loop (addMenuUi, runMenu)
import Ribosome.Menu.MenuTest (MenuTestIOStack, MenuTestStack, MenuTestWith, menuTestLoop, runTestMenu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), PromptText)
import Ribosome.Menu.Prompt.Data.PromptConfig (startInsert)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Test.Error (testError)
import Ribosome.Test.Wait (assertWait)

enqueueItems ::
  Members [Hedgehog IO, Queue [Text]] r =>
  InterpreterFor (ScopedMenuUi p ()) r
enqueueItems =
  interpretScopedR_ (const unit) \ () -> \case
    MenuUi.Render menu ->
      evalMaybe . resultToMaybe =<< Queue.writeTimeout (Seconds 5) (MenuItem.text . Entry.item <$> menu ^. #entries . to sortEntries)
    MenuUi.RenderPrompt _ _ ->
      unit
    MenuUi.PromptEvent ->
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
    ScopedMenuUi () (),
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
  enqueueItems

type PromptTest i =
  Consume MenuEvent :
  MenuTestWith (Modal Filter i) () ++
  Menus () (Modal Filter i) :
  UiMenus () () (Modal Filter i) :
  UiMenus () () (Modal Filter i) !! RpcError :
  MenuFilter (FilterMode Filter) :
  Stop RpcError :
  SimpleTestMenu ++
  MenuTestStack i ()

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
  interpretMenus $
  resumeReportFail $
  addMenuUi () do
    items <- ask
    runMenu items (modal Fuzzy) $
      bundleMenuEngine $
      menuTestLoop @_ @_ @(Modal Filter i) startInsert (const (Just enqueuePrompt)) $
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

awaitCurrent ::
  MenuState s =>
  HasCallStack =>
  Members [Menu s, Hedgehog IO, ChronosTime, Error Failure, Race, Async, Embed IO] r =>
  [Text] ->
  Sem r ()
awaitCurrent target =
  withFrozenCallStack do
    assertWait currentEntries (assertEq target)
