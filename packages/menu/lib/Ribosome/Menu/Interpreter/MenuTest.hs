module Ribosome.Menu.Interpreter.MenuTest where

import Conc (Restoration, interpretQueueTBM, interpretSync, resultToMaybe, timeout_, withAsync_)
import Exon (exon)
import Polysemy.Chronos (ChronosTime)
import qualified Queue
import qualified Streamly.Prelude as Stream
import qualified Sync

import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Mapping, NewItems, PromptEdit, PromptNavigation))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Data.NvimMenuConfig (NvimMenuConfig (NvimMenuConfig))
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer (MenuConsumerEvent), menuConsumerEvent)
import Ribosome.Menu.Effect.MenuState (MenuState)
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer, withMenuRenderer)
import Ribosome.Menu.Effect.MenuTest (MenuTest (ItemsDone, Result, SendItem, SendPrompt), sendStaticItems)
import Ribosome.Menu.Effect.PromptControl (PromptControl, waitPromptListening)
import Ribosome.Menu.Effect.PromptEvents (PromptEvents)
import Ribosome.Menu.Effect.PromptInput (PromptInput)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer, withPrompt)
import Ribosome.Menu.Filters (fuzzyMonotonic)
import Ribosome.Menu.Interpreter.PromptControl (interpretPromptControl)
import Ribosome.Menu.Interpreter.PromptEvents (interpretPromptEventsDefault)
import Ribosome.Menu.Interpreter.PromptInput (interpretPromptInputQueue)
import Ribosome.Menu.Main (menuMain)
import Ribosome.Menu.Nvim (interpretNvimMenu)
import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)
import Ribosome.Menu.Prompt.Data.PromptFlag (PromptFlag)
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
  ∀ i u a r .
  Show i =>
  Show u =>
  TimeUnit u =>
  Members [Queue InputEvent, Queue (MenuItem i), Queue MenuEvent, Sync (MenuResult a)] r =>
  u ->
  (∀ x . Text -> Sem r x) ->
  InterpreterFor (MenuTest i a) r
interpretMenuTest timeout failure =
  interpret \case
    SendItem item ->
      noteFail [exon|Could not send #{show item}|] . resultToMaybe =<< Queue.writeTimeout timeout item
    ItemsDone -> do
      Queue.close @(_ _)
      waitEvent timeout failure \case
        NewItems -> True
        _ -> False
    SendPrompt wait e -> do
      noteFail [exon|Could not send #{show e}|] . resultToMaybe =<< Queue.writeTimeout timeout e
      when wait do
        waitEvent timeout failure \case
          PromptEdit -> True
          PromptNavigation -> True
          Mapping _ -> True
          _ -> False
    Result ->
      noteFail [exon|No result was produced within #{show timeout}|] =<< Sync.wait timeout
  where
    noteFail :: ∀ x . Text -> Maybe x -> Sem r x
    noteFail msg =
      maybe (failure msg) pure

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
  Show u =>
  TimeUnit u =>
  Members [MenuConsumer o, Resource, Race, Embed IO] r =>
  u ->
  (∀ x . Text -> Sem r x) ->
  InterpretersFor [PromptInput, MenuTest i a, Queue MenuEvent, Queue InputEvent, Queue (MenuItem i), Sync (MenuResult a)] r
runMenuTest timeout failure =
  interpretSync .
  interpretQueueTBM @(MenuItem _) 64 .
  interpretQueueTBM @InputEvent 64 .
  interpretQueueTBM @MenuEvent 64 .
  interpretMenuTest timeout (insertAt @0 . failure) .
  interpretPromptInputQueue .
  interceptMenuConsumerQueue timeout (insertAt @0 . failure)

withTestMenu ::
  Show result =>
  TimeUnit u =>
  Members [PromptControl, PromptInput, PromptEvents, PromptRenderer] r =>
  Members [Sync (MenuResult result), MenuState i, MenuConsumer result, MenuRenderer i] r =>
  Members [ChronosTime, Log, Mask Restoration, Race, Resource, Async, Embed IO, Final IO] r =>
  u ->
  (∀ x . Text -> Sem r x) ->
  MenuConfig i ->
  Sem r a ->
  Sem r a
withTestMenu timeout failure conf sem =
  withAsync_ (Sync.putWait timeout =<< menuMain conf) do
    timeout_ (failure "prompt didn't start") timeout waitPromptListening
    sem

type MenuTestEffects i result =
  [
    PromptInput,
    MenuTest i result,
    Queue MenuEvent,
    Queue InputEvent,
    Queue (MenuItem i),
    Sync (MenuResult result),
    PromptEvents,
    PromptRenderer,
    PromptControl
  ]

menuTest ::
  ∀ i result pres u r .
  Show result =>
  Show i =>
  Show u =>
  TimeUnit u =>
  Members [MenuState i, MenuConsumer result, MenuRenderer i, Scoped pres PromptRenderer] r =>
  Members [ChronosTime, Log, Mask Restoration, Race, Resource, Async, Resource, Embed IO, Final IO] r =>
  [PromptFlag] ->
  MenuItemFilter i ->
  u ->
  (∀ x . Text -> Sem r x) ->
  InterpretersFor (MenuTestEffects i result) r
menuTest flags itemFilter timeout failure sem =
  interpretPromptControl $ withPrompt $ interpretPromptEventsDefault flags $ runMenuTest timeout (insertAt @0 . failure) do
    items <- queueStream
    withTestMenu timeout (insertAt @0 . failure) (MenuConfig items (Just itemFilter) flags) sem

staticMenuTest ::
  ∀ i result pres u r .
  Show result =>
  Show i =>
  Show u =>
  TimeUnit u =>
  Members [MenuState i, MenuConsumer result, MenuRenderer i, Scoped pres PromptRenderer] r =>
  Members [ChronosTime, Log, Mask Restoration, Race, Async, Resource, Embed IO, Final IO] r =>
  [PromptFlag] ->
  MenuItemFilter i ->
  [MenuItem i] ->
  u ->
  (∀ x . Text -> Sem r x) ->
  InterpretersFor (MenuTestEffects i result) r
staticMenuTest flags itemFilter items timeout failure sem =
  menuTest flags itemFilter timeout failure do
    sendStaticItems items
    sem

staticNvimMenuTest ::
  ∀ i result u r .
  Show result =>
  Show i =>
  Show u =>
  TimeUnit u =>
  Members [MenuState i, MenuConsumer result, Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch] r =>
  Members [ChronosTime, Log, Mask Restoration, Race, Async, Resource, Embed IO, Final IO] r =>
  NvimMenuConfig i ->
  u ->
  (∀ x . Text -> Sem r x) ->
  InterpretersFor (MenuTestEffects i result) r
staticNvimMenuTest conf@(NvimMenuConfig (MenuConfig items itemFilter flags) _) timeout failure sem = do
  interpretNvimMenu conf $ withMenuRenderer do
    staticItems <- embed (Stream.toList items)
    staticMenuTest flags (fromMaybe fuzzyMonotonic itemFilter) staticItems timeout (insertAt @0 . failure) (insertAt @9 sem)
