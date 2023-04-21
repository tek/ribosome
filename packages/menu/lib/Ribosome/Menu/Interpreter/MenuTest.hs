module Ribosome.Menu.Interpreter.MenuTest where

import Conc (Consume, interpretAtomic, interpretQueueTBM, interpretSync, resultToMaybe, timeout_)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Exon (exon)
import Polysemy.Chronos (ChronosTime, interpretTimeChronos)
import qualified Queue
import qualified Sync
import Time (NanoSeconds, convert)

import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Exhausted, Rendered))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Effect.MenuTest (MenuTest (..))
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Effect.MenuUi (MenuUi)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Prompt.Run (pristinePrompt)

newtype TestTimeout =
  TestTimeout { unTestTimeout :: NanoSeconds }
  deriving stock (Eq, Show)

failTimeout ::
  Members [Reader TestTimeout, Fail, Race] r =>
  Text ->
  Sem r a ->
  Sem r a
failTimeout desc ma = do
  TestTimeout timeout <- ask
  timeout_ (fail [exon|timeout: #{toString desc}|]) timeout ma

noteFail ::
  Member Fail r =>
  Text ->
  Maybe a ->
  Sem r a
noteFail desc =
  fromMaybeA (fail [exon|value is Nothing: #{toString desc}|])

failQueue ::
  Members [Reader TestTimeout, Fail, Race] r =>
  Text ->
  Sem r (QueueResult a) ->
  Sem r a
failQueue desc =
  noteFail desc . resultToMaybe <=< failTimeout desc

interceptMenuQueue ::
  ∀ r a .
  Members [MenuUi, Queue PromptEvent, Reader TestTimeout, Fail, Race] r =>
  Sem r a ->
  Sem r a
interceptMenuQueue =
  interceptH \case
    MenuUi.PromptEvent -> do
      pureT =<< failQueue "MenuUi PromptEvent" Queue.read
    MenuUi.RenderPrompt c p ->
      pureT =<< MenuUi.renderPrompt c p
    MenuUi.Render m ->
      pureT =<< MenuUi.render m

data WaitEvent =
  Requested Text
  |
  Received MenuEvent
  |
  Satisfied Text
  deriving stock (Eq, Show)

data WaitState =
  WaitState {
    exhausted :: Bool,
    events :: [WaitEvent]
  }
  deriving stock (Eq, Show, Generic)

pushWaitEvent ::
  Member (AtomicState WaitState) r =>
  WaitEvent ->
  Sem r ()
pushWaitEvent e =
  atomicModify' (#events %~ (e :))

consumeMenuEvent ::
  Members [Consume MenuEvent, AtomicState WaitState] r =>
  Sem r MenuEvent
consumeMenuEvent =
  consume >>= tap \case
    MenuEvent.Exhausted -> atomicModify (#exhausted .~ True)
    _ -> unit

waitEventPredM ::
  Members [Reader TestTimeout, Consume MenuEvent, AtomicState WaitState, Fail, Race] r =>
  Text ->
  (MenuEvent -> Sem r Bool) ->
  Sem r ()
waitEventPredM desc match = do
  pushWaitEvent (Requested desc)
  failTimeout [exon|MenuEvent not published: #{desc}|] spin
  pushWaitEvent (Satisfied desc)
  where
    spin = do
      e <- consumeMenuEvent
      pushWaitEvent (Received e)
      unlessM (match e) spin

waitEventPred ::
  Members [Reader TestTimeout, Consume MenuEvent, AtomicState WaitState, Fail, Race] r =>
  Text ->
  (MenuEvent -> Bool) ->
  Sem r ()
waitEventPred desc match =
  waitEventPredM desc (pure . match)

waitEvent ::
  Members [Reader TestTimeout, Consume MenuEvent, AtomicState WaitState, Fail, Race] r =>
  Text ->
  MenuEvent ->
  Sem r ()
waitEvent desc target =
  waitEventPred [exon|#{desc} (#{show target})|] (target ==)

waitEvents ::
  Members [Reader TestTimeout, Consume MenuEvent, AtomicState WaitState, Fail, Race] r =>
  Text ->
  Set MenuEvent ->
  Sem r ()
waitEvents desc targets =
  evalState targets $ waitEventPredM [exon|#{desc} (#{Text.intercalate ", " (show <$> toList targets)})|] \ e -> do
    modify' (Set.delete e)
    gets Set.null

waitItemsDone ::
  Members [Reader TestTimeout, Consume MenuEvent, AtomicState WaitState, Fail, Race] r =>
  Text ->
  Sem r ()
waitItemsDone desc =
  unlessM (atomicView #exhausted) do
    waitEvents desc [Exhausted, Rendered]

updatePrompt ::
  Member (AtomicState Prompt) r =>
  PromptEvent ->
  Sem r Bool
updatePrompt = \case
  PromptEvent.Update new ->
    True <$ atomicPut new
  _ ->
    pure False

waitPromptUpdate ::
  Members [Reader TestTimeout, Consume MenuEvent, AtomicState WaitState, Fail, Race] r =>
  Text ->
  Sem r ()
waitPromptUpdate desc =
  waitEventPred [exon|prompt update (#{desc})|] \case
    MenuEvent.Query _ -> True
    _ -> False

nextEvent ::
  Members [Reader TestTimeout, Consume MenuEvent, Fail, Race] r =>
  Sem r MenuEvent
nextEvent =
  failTimeout "next event" consume

sendPromptEvent ::
  Members [Reader TestTimeout, Consume MenuEvent, Queue PromptEvent, AtomicState Prompt, AtomicState WaitState, Fail, Race] r =>
  Bool ->
  PromptEvent ->
  Sem r ()
sendPromptEvent wait e = do
  isUpdate <- updatePrompt e
  failTimeout [exon|send #{show e}|] (Queue.write e)
  when (wait && isUpdate) do
    waitPromptUpdate [exon|send #{show e}|]

withPromptInit ::
  Members [Reader TestTimeout, Consume MenuEvent, Queue PromptEvent, AtomicState Prompt, Fail, Race] r =>
  Sem r a ->
  Sem r a
withPromptInit sem = do
  p <- atomicGet
  failTimeout "initialize prompt" (Queue.write (PromptEvent.Update p))
  sem

interpretMenuTestQueuesWith ::
  ∀ i a r .
  Show i =>
  Members [Queue PromptEvent, Queue (MenuItem i), Sync (MenuResult a), Reader TestTimeout, Fail] r =>
  Members [AtomicState Prompt, AtomicState WaitState, Consume MenuEvent, Race, Log] r =>
  InterpreterFor (MenuTest i a) r
interpretMenuTestQueuesWith =
  withPromptInit .
  interpret \case
    SendItem item ->
      failTimeout (show item) (Queue.write item)
    ItemsDone desc -> do
      Queue.close @(MenuItem _)
      waitItemsDone [exon|ItemsDone (#{desc})|]
    WaitItemsDone desc ->
      waitItemsDone [exon|WaitItemsDone (#{desc})|]
    SendPromptEvent wait e ->
      sendPromptEvent wait e
    WaitEventPred desc f ->
      waitEventPred desc f
    WaitEvent desc e ->
      waitEventPred desc (e ==)
    WaitEvents desc es ->
      waitEvents desc es
    Result ->
      failTimeout "menu result" Sync.block
    NextEvent ->
      failTimeout "next event" consume
    Quit ->
      sendPromptEvent False (PromptEvent.Quit Nothing)

interpretMenuTestQueues ::
  ∀ i a r .
  Show i =>
  Members [EventConsumer MenuEvent, AtomicState Prompt, AtomicState WaitState] r =>
  Members [Queue PromptEvent, Queue (MenuItem i), Sync (MenuResult a), Reader TestTimeout, Log, Fail, Race] r =>
  PromptConfig ->
  InterpretersFor [MenuTest i a, Consume MenuEvent] r
interpretMenuTestQueues pconf =
  subscribe @MenuEvent .
  runReader pconf .
  interpretMenuTestQueuesWith .
  raiseUnder

type MenuTestResources i result =
  [
    Queue PromptEvent,
    Queue (MenuItem i),
    Sync (MenuResult result),
    Reader TestTimeout,
    AtomicState Prompt,
    AtomicState WaitState,
    ChronosTime
  ]

interpretMenuTestResources ::
  TimeUnit u =>
  Members [Resource, Race, Embed IO] r =>
  u ->
  PromptConfig ->
  InterpretersFor (MenuTestResources i a) r
interpretMenuTestResources timeout pconf =
  interpretTimeChronos .
  interpretAtomic (WaitState False mempty) .
  interpretAtomic (pristinePrompt pconf) .
  runReader (TestTimeout (convert timeout)) .
  interpretSync .
  interpretQueueTBM @(MenuItem _) 64 .
  interpretQueueTBM @PromptEvent 64

interpretMenuTest ::
  Show i =>
  Members (MenuTestResources i result) r =>
  Members [MenuUi, Log, Fail, Resource, Race, Embed IO] r =>
  Member (EventConsumer MenuEvent) r =>
  Bool ->
  PromptConfig ->
  InterpretersFor [MenuTest i result, Consume MenuEvent] r
interpretMenuTest nativePrompt pconf =
  interpretMenuTestQueues pconf .
  (if nativePrompt then id else interceptMenuQueue)
