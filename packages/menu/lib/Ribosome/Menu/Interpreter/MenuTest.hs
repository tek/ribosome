module Ribosome.Menu.Interpreter.MenuTest where

import Conc (Consume, interpretAtomic, interpretQueueTBM, interpretSync, resultToMaybe, timeout_)
import qualified Data.Text as Text
import Exon (exon)
import Polysemy.Chronos (ChronosTime, interpretTimeChronos)
import qualified Queue
import qualified Sync
import Time (Seconds (Seconds), convert)

import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Exhausted, Rendered))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import qualified Ribosome.Menu.Data.TestMenuConfig
import Ribosome.Menu.Data.TestMenuConfig (TestMenuConfig, TestTimeout (TestTimeout))
import qualified Ribosome.Menu.Effect.MenuTest as MenuTest
import Ribosome.Menu.Effect.MenuTest (MenuTest (..))
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Effect.MenuUi (MenuUi)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Stream.Util (queueStream)

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
  ∀ i result r a .
  Members [MenuTest i result, MenuUi] r =>
  Sem r a ->
  Sem r a
interceptMenuQueue =
  interceptH \case
    MenuUi.PromptEvent ->
      pureT =<< MenuTest.promptEvent
    MenuUi.RenderPrompt c p ->
      pureT =<< MenuUi.renderPrompt c p
    MenuUi.Render m ->
      pureT =<< MenuUi.render m
    MenuUi.PromptScratch ->
      pureT =<< MenuUi.promptScratch
    MenuUi.StatusScratch ->
      pureT =<< MenuUi.statusScratch
    MenuUi.ItemsScratch ->
      pureT =<< MenuUi.itemsScratch

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

waitEventsPred ::
  Members [Reader TestTimeout, Consume MenuEvent, AtomicState WaitState, Fail, Race] r =>
  Text ->
  [MenuEvent -> Bool] ->
  Sem r ()
waitEventsPred desc targets =
  evalState targets $ waitEventPredM desc \ e -> do
    modify' (removeEvent e)
    gets null
  where
    removeEvent e = \case
      [] -> []
      (h : t) | h e -> t
      (h : t) -> h : removeEvent e t

waitEvents ::
  Members [Reader TestTimeout, Consume MenuEvent, AtomicState WaitState, Fail, Race] r =>
  Text ->
  [MenuEvent] ->
  Sem r ()
waitEvents desc evs =
  waitEventsPred desc ((==) <$> evs)

waitItemsDone ::
  Members [Reader TestTimeout, Consume MenuEvent, AtomicState WaitState, Fail, Race] r =>
  Text ->
  Sem r ()
waitItemsDone desc =
  unlessM (atomicView #exhausted) do
    waitEvents desc [Exhausted, Rendered]

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
  Members [Reader TestTimeout, Consume MenuEvent, Queue PromptEvent, AtomicState WaitState, Fail, Race] r =>
  Bool ->
  PromptEvent ->
  Sem r ()
sendPromptEvent wait e = do
  failTimeout [exon|send #{show e}|] (Queue.write e)
  when wait do
    waitPromptUpdate [exon|send #{show e}|]

handleMenuTest ::
  ∀ i result m r a .
  Show i =>
  Members [Queue PromptEvent, Queue (MenuItem i), Sync (MenuResult result), Reader TestTimeout, Fail] r =>
  Members [AtomicState WaitState, Consume MenuEvent, Race, Log, Final IO] r =>
  TestMenuConfig i ->
  MenuTest i result m a ->
  Sem r a
handleMenuTest conf = \case
  ItemsStream
    | Just items <- conf.items
    -> pure items
    | otherwise
    -> queueStream
  Config ->
    pure conf
  PromptEvent ->
    failQueue "MenuUi PromptEvent" Queue.read
  SendItem item ->
    failTimeout (show item) (Queue.write item)
  ItemsDone desc -> do
    Queue.close @(MenuItem _)
    waitItemsDone [exon|ItemsDone (#{desc})|]
  WaitItemsDone desc ->
    waitItemsDone [exon|WaitItemsDone (#{desc})|]
  SendPromptEvent wait e ->
    sendPromptEvent wait e
  WaitEventsPred desc f ->
    waitEventsPred desc f
  Result ->
    failTimeout "menu result" Sync.block
  NextEvent ->
    failTimeout "next event" consume
  LoopFinished result -> do
    TestTimeout timeout <- ask
    Sync.putWait timeout result
  Timeout ->
    ask
  Quit ->
    sendPromptEvent False (PromptEvent.Quit Nothing)

type MenuTestResources i result =
  [
    Consume MenuEvent,
    Queue PromptEvent,
    Queue (MenuItem i),
    Sync (MenuResult result),
    Reader TestTimeout,
    AtomicState WaitState,
    ChronosTime
  ]

interpretMenuTestResources ::
  Members [EventConsumer MenuEvent, Resource, Race, Embed IO] r =>
  TestTimeout ->
  InterpretersFor (MenuTestResources i a) r
interpretMenuTestResources timeout =
  interpretTimeChronos .
  interpretAtomic (WaitState False mempty) .
  runReader timeout .
  interpretSync .
  interpretQueueTBM @(MenuItem _) 64 .
  interpretQueueTBM @PromptEvent 64 .
  subscribe

withEventLog ::
  Members [Queue PromptEvent, Consume MenuEvent, AtomicState WaitState, Reader TestTimeout] r =>
  Members [Fail, Resource, Race, Embed IO] r =>
  TestMenuConfig i ->
  Sem r a ->
  Sem r a
withEventLog conf =
  flip onException do
    sendPromptEvent False (PromptEvent.Quit Nothing)
    -- want this to always be printed, but using Log.crit sends it to nvim as well, and adding StderrLog just for this
    -- seems overblown
    events <- atomicGets (.events)
    embed do
      putStrLn (toString (format events))
      putStrLn (show conf)
  where
    format evs =
      Text.unlines ("wait events:" : (("  " <>) . show <$> reverse evs))

menuTestScope ::
  Members [EventConsumer MenuEvent, Resource, Race, Fail, Embed IO] r =>
  TestMenuConfig i ->
  (TestMenuConfig i -> Sem (MenuTestResources i result ++ r) a) ->
  Sem r a
menuTestScope conf use =
  interpretMenuTestResources (fromMaybe (TestTimeout (convert (Seconds 3))) conf.timeout) do
    withEventLog conf $ use conf

interpretMenuTests ::
  ∀ i result r .
  Show i =>
  Members [EventConsumer MenuEvent, Log, Resource, Race, Fail, Embed IO, Final IO] r =>
  InterpreterFor (Scoped (TestMenuConfig i) (MenuTest i result)) r
interpretMenuTests =
  interpretScopedWith @(MenuTestResources i result) menuTestScope \ conf -> handleMenuTest conf
