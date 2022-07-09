module Ribosome.Menu.Interpreter.MenuTest where

import Conc (interpretQueueTBM, interpretSync, resultToMaybe)
import Exon (exon)
import Polysemy.Chronos (ChronosTime, interpretTimeChronos)
import qualified Queue
import qualified Sync
import Time (NanoSeconds, convert)

import Ribosome.Host.Interpret (with)
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Exhausted, Mapping, NewItems, PromptEdit, PromptNavigation))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer (MenuConsumerEvent), menuConsumerEvent)
import Ribosome.Menu.Effect.MenuTest (MenuTest (ItemsDone, Result, SendItem, SendPrompt, WaitItemsDone))
import Ribosome.Menu.Effect.PromptInput (PromptInput)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer, withPrompt)
import Ribosome.Menu.Interpreter.PromptInput (interpretPromptInputQueue)
import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)

newtype TestTimeout =
  TestTimeout { unTestTimeout :: NanoSeconds }
  deriving stock (Eq, Show)

waitEvent ::
  TimeUnit u =>
  Members [Queue MenuEvent, Fail] r =>
  Text ->
  u ->
  (MenuEvent -> Bool) ->
  Sem r ()
waitEvent desc timeout match =
  spin
  where
    spin = do
      e <- maybe (fail err) pure . resultToMaybe =<< Queue.readTimeout timeout
      unless (match e) spin
    err =
      [exon|expected MenuEvent did not appear: #{toString desc}|]

interpretMenuTestQueues ::
  ∀ i a r .
  Show i =>
  Members [Queue InputEvent, Queue (MenuItem i), Queue MenuEvent, Sync (MenuResult a), Reader TestTimeout, Fail] r =>
  InterpreterFor (MenuTest i a) r
interpretMenuTestQueues =
  with ask \ (TestTimeout timeout) ->
    interpret \case
      SendItem item ->
        noteFail [exon|Could not send #{show item}|] . resultToMaybe =<< Queue.writeTimeout timeout item
      ItemsDone -> do
        Queue.close @(_ _)
        waitEvent "NewItems" timeout \case
          NewItems -> True
          _ -> False
      WaitItemsDone -> do
        waitEvent "Exhausted" timeout \case
          Exhausted -> True
          _ -> False
      SendPrompt wait e -> do
        noteFail [exon|Could not send #{show e}|] . resultToMaybe =<< Queue.writeTimeout timeout e
        when wait do
          waitEvent "prompt update" timeout \case
            PromptEdit -> True
            PromptNavigation -> True
            Mapping _ -> True
            _ -> False
      Result ->
        noteFail [exon|No result was produced within #{show timeout}|] =<< Sync.wait timeout
    where
      noteFail :: ∀ x . String -> Maybe x -> Sem r x
      noteFail msg =
        maybe (fail msg) pure

interceptMenuConsumerQueue ::
  Members [MenuConsumer o, Queue MenuEvent, Reader TestTimeout, Fail] r =>
  Sem r a ->
  Sem r a
interceptMenuConsumerQueue =
  intercept \case
    MenuConsumerEvent e -> do
      TestTimeout timeout <- ask
      maybe (fail [exon|Could not enqueue #{show e}|]) pure . resultToMaybe =<< Queue.writeTimeout timeout e
      menuConsumerEvent e

type MenuTestResources i result =
  [
    Queue MenuEvent,
    Queue InputEvent,
    Queue (MenuItem i),
    Sync (MenuResult result),
    Reader TestTimeout,
    ChronosTime
  ]

interpretMenuTestResources ::
  TimeUnit u =>
  Members [Resource, Race, Embed IO] r =>
  u ->
  InterpretersFor (MenuTestResources i a) r
interpretMenuTestResources timeout =
  interpretTimeChronos .
  runReader (TestTimeout (convert timeout)) .
  interpretSync .
  interpretQueueTBM @(MenuItem _) 64 .
  interpretQueueTBM @InputEvent 64 .
  interpretQueueTBM @MenuEvent 64

interpretMenuTest ::
  Show i =>
  Members (MenuTestResources i result) r =>
  Members [MenuConsumer result, Scoped pres PromptRenderer, Fail, Resource, Race, Embed IO] r =>
  InterpretersFor [PromptInput, MenuTest i result, PromptRenderer] r
interpretMenuTest =
  withPrompt .
  interpretMenuTestQueues .
  interpretPromptInputQueue .
  interceptMenuConsumerQueue
