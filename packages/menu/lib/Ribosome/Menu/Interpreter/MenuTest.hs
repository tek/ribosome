module Ribosome.Menu.Interpreter.MenuTest where

import Conc (Restoration, interpretQueueTBM, resultToMaybe, withAsync_)
import Exon (exon)
import qualified Queue
import qualified Sync

import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import Ribosome.Menu.Data.MenuEvent (MenuEvent (NewItems))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter)
import Ribosome.Menu.Data.MenuState (MenuStack)
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer (MenuConsumerEvent), menuConsumerEvent)
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer)
import Ribosome.Menu.Effect.MenuTest (MenuTest (ItemsDone, SendItem, SendPrompt))
import Ribosome.Menu.Effect.PromptEvents (PromptEvents)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer, withPrompt)
import Ribosome.Menu.Interpreter.PromptEvents (interpretPromptEventsDefault)
import Ribosome.Menu.Main (menuMain)
import Ribosome.Menu.Prompt (PromptFlag, PromptListening (PromptListening), queuePrompt)
import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)
import Ribosome.Menu.Stream.Util (queueStream)

waitEvent ::
  TimeUnit u =>
  Member (Queue MenuEvent) r =>
  u ->
  (∀ x . Text -> Sem r x) ->
  (MenuEvent -> Bool) ->
  Sem r ()
waitEvent timeout failure match =
  spin
  where
    spin = do
      e <- maybe (failure [exon|expected MenuEvent did not appear|]) pure . resultToMaybe =<< Queue.readTimeout timeout
      unless (match e) spin

interpretMenuTest ::
  Show i =>
  TimeUnit u =>
  Members [Queue PromptInputEvent, Queue (MenuItem i), Queue MenuEvent] r =>
  u ->
  (∀ x . Text -> Sem r x) ->
  InterpreterFor (MenuTest i) r
interpretMenuTest timeout failure =
  interpret \case
    SendItem item ->
      maybe (failure [exon|Could not send #{show item}|]) pure . resultToMaybe =<< Queue.writeTimeout timeout item
    ItemsDone -> do
      Queue.close @(_ _)
      waitEvent timeout failure \case
        NewItems -> True
        _ -> False
    SendPrompt e ->
      maybe (failure [exon|Could not send #{show e}|]) pure . resultToMaybe =<< Queue.writeTimeout timeout e

interceptMenuConsumerQueue ::
  TimeUnit u =>
  Members [MenuConsumer o, Queue MenuEvent] r =>
  u ->
  (∀ x . Text -> Sem r x) ->
  Sem r a ->
  Sem r a
interceptMenuConsumerQueue timeout failure =
  intercept \case
    MenuConsumerEvent e -> do
      maybe (failure [exon|Could not enqueue #{show e}|]) pure . resultToMaybe =<< Queue.writeTimeout timeout e
      menuConsumerEvent e

runMenuTest ::
  Show i =>
  TimeUnit u =>
  Members [MenuConsumer o, Resource, Race, Embed IO] r =>
  u ->
  (∀ x . Text -> Sem r x) ->
  InterpretersFor [MenuTest i, Queue MenuEvent, Queue PromptInputEvent, Queue (MenuItem i)] r
runMenuTest timeout failure =
  interpretQueueTBM @(MenuItem _) 64 .
  interpretQueueTBM @PromptInputEvent 64 .
  interpretQueueTBM @MenuEvent 64 .
  interpretMenuTest timeout (insertAt @0 . failure) .
  interceptMenuConsumerQueue timeout (insertAt @0 . failure)

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
  InterpretersFor [MenuTest i, Queue MenuEvent, Queue PromptInputEvent, Queue (MenuItem i), PromptEvents, PromptRenderer] r
menuTest flags itemFilter timeout failure sem =
  withPrompt $ interpretPromptEventsDefault flags $ runMenuTest timeout (insertAt @0 . failure) do
    prompts <- queuePrompt flags
    items <- queueStream
    withTestMenu timeout (insertAt @0 . failure) (MenuConfig items itemFilter prompts) sem
