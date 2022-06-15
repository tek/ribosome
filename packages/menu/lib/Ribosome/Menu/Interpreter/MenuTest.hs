module Ribosome.Menu.Interpreter.MenuTest where

import Conc (Restoration, interpretQueueTBM, resultToMaybe, withAsync_)
import Exon (exon)
import qualified Queue
import qualified Sync

import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter)
import Ribosome.Menu.Data.MenuState (MenuStack)
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer)
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer)
import Ribosome.Menu.Effect.MenuTest (MenuTest (ItemsDone, SendItem, SendPrompt))
import Ribosome.Menu.Effect.PromptEvents (PromptEvents)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer, withPrompt)
import Ribosome.Menu.Interpreter.PromptEvents (interpretPromptEventsDefault)
import Ribosome.Menu.Main (menuMain)
import Ribosome.Menu.Prompt (PromptFlag, PromptListening (PromptListening), queuePrompt)
import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)
import Ribosome.Menu.Stream.Util (queueStream)

interpretMenuTest ::
  Show i =>
  TimeUnit u =>
  Members [Queue PromptInputEvent, Queue (MenuItem i)] r =>
  u ->
  (∀ x . Text -> Sem r x) ->
  InterpreterFor (MenuTest i) r
interpretMenuTest timeout failure =
  interpret \case
    SendItem item ->
      maybe (failure [exon|Could not send #{show item}|]) pure . resultToMaybe =<< Queue.writeTimeout timeout item
    ItemsDone ->
      Queue.close @(_ _)
    SendPrompt e ->
      maybe (failure [exon|Could not send #{show e}|]) pure . resultToMaybe =<< Queue.writeTimeout timeout e

runMenuTest ::
  Show i =>
  TimeUnit u =>
  Members [Resource, Race, Embed IO] r =>
  u ->
  (∀ x . Text -> Sem r x) ->
  InterpretersFor [MenuTest i, Queue PromptInputEvent, Queue (MenuItem i)] r
runMenuTest timeout failure =
  interpretQueueTBM @(MenuItem _) 64 .
  interpretQueueTBM @PromptInputEvent 64 .
  interpretMenuTest timeout (insertAt @0 . failure)

withTestMenu ::
  Show result =>
  TimeUnit u =>
  Members (MenuStack i) r =>
  Members [MenuConsumer result, MenuRenderer i, PromptEvents, PromptRenderer] r =>
  Members [Sync PromptListening, Log, Mask Restoration, Race, Resource, Async, Embed IO, Final IO] r =>
  u ->
  (∀ x . Text -> Sem r x) ->
  MenuConfig i ->
  Sem r a ->
  Sem r a
withTestMenu timeout failure conf sem =
  withAsync_ (void (menuMain conf)) do
    PromptListening <- maybe (failure "prompt didn't start") pure =<< Sync.wait timeout
    sem

menuTest ::
  ∀ i result pres u r .
  Show result =>
  Show i =>
  TimeUnit u =>
  Members (MenuStack i) r =>
  Members [MenuConsumer result, MenuRenderer i, Scoped pres PromptRenderer] r =>
  Members [Sync PromptListening, Log, Mask Restoration, Race, Resource, Async, Embed IO, Final IO] r =>
  [PromptFlag] ->
  MenuItemFilter i ->
  u ->
  (∀ x . Text -> Sem r x) ->
  InterpretersFor [MenuTest i, Queue PromptInputEvent, Queue (MenuItem i), PromptEvents, PromptRenderer] r
menuTest flags itemFilter timeout failure sem =
  withPrompt $ interpretPromptEventsDefault flags $ runMenuTest timeout (insertAt @0 . failure) do
    prompts <- queuePrompt flags
    items <- queueStream
    withTestMenu timeout (insertAt @0 . failure) (MenuConfig items itemFilter prompts) sem
