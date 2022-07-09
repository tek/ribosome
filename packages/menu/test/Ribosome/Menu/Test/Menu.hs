module Ribosome.Menu.Test.Menu where

import Conc (Restoration, interpretQueueTBM, resultToMaybe)
import Polysemy.Chronos (ChronosTime, interpretTimeChronos)
import Polysemy.Log (interpretLogNull)
import Polysemy.Test (Hedgehog, assertEq, assertJust, evalMaybe)
import qualified Queue
import Time (Seconds (Seconds))

import Ribosome.Menu.Action (menuIgnore, menuQuit)
import Ribosome.Menu.Combinators (sortedEntries)
import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entry)
import Ribosome.Menu.Data.MenuAction (MenuAction)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer (MenuConsumerEvent))
import qualified Ribosome.Menu.Effect.MenuRenderer as MenuRenderer
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer)
import Ribosome.Menu.Effect.MenuState (MenuState, readPrompt)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer)
import Ribosome.Menu.Interpreter.PromptRenderer (interpretPromptRendererNull)
import Ribosome.Menu.MenuTest (MenuTestEffects, MenuTestStack, runTestMenuWith, testMenuRender)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptFlag (PromptFlag (StartInsert))

enqueueItems ::
  Members [Hedgehog IO, Queue [Entry i]] r =>
  InterpreterFor (MenuRenderer i) r
enqueueItems =
  interpret \case
    MenuRenderer.MenuRender menu -> do
      evalMaybe . resultToMaybe =<< Queue.writeTimeout (Seconds 5) (menu ^. sortedEntries)

enqueuePrompt ::
  ∀ i r .
  Members [MenuState i, Hedgehog IO, Queue Prompt, Resource, Embed IO] r =>
  InterpreterFor (MenuConsumer ()) r
enqueuePrompt =
  interpret \case
    MenuConsumerEvent MenuEvent.PromptEdit ->
      enq
    MenuConsumerEvent MenuEvent.PromptNavigation ->
      enq
    MenuConsumerEvent (MenuEvent.Quit _) ->
      menuQuit
    MenuConsumerEvent _ ->
      menuIgnore
  where
    enq :: Sem r (Maybe (MenuAction ()))
    enq = do
      Queue.write =<< readPrompt
      menuIgnore

type SimpleTestMenu i =
  [
    Scoped () PromptRenderer,
    MenuRenderer i,
    ChronosTime,
    Log,
    Queue [Entry i],
    Queue Prompt
  ]

runSimpleTestMenu ::
  ∀ i r .
  Members [Hedgehog IO, Resource, Race, Mask Restoration, Embed IO, Final IO] r =>
  InterpretersFor (SimpleTestMenu i) r
runSimpleTestMenu =
  interpretQueueTBM @Prompt 64 .
  interpretQueueTBM @[Entry i] 64 .
  interpretLogNull .
  interpretTimeChronos .
  enqueueItems .
  interpretPromptRendererNull

type PromptTest i =
  MenuTestEffects i () ++ MenuConsumer () : SimpleTestMenu i ++ MenuTestStack i ()

promptTest ::
  Show i =>
  Members [Hedgehog IO, Fail, Log, Resource, Race, Mask Restoration, Async, Embed IO, Final IO] r =>
  InterpretersFor (PromptTest i) r
promptTest =
  runTestMenuWith (Seconds 5) [StartInsert] True .
  runSimpleTestMenu .
  enqueuePrompt .
  testMenuRender

assertPrompt ::
  Members [Hedgehog IO, Queue Prompt] r =>
  Prompt ->
  Sem r ()
assertPrompt p =
  assertJust p . resultToMaybe =<< Queue.readTimeout (Seconds 5)

currentItems ::
  Members [Queue [Entry i], Hedgehog IO] r =>
  Sem r [Text]
currentItems =
  fmap (MenuItem.text . Entry.item) <$> (evalMaybe . resultToMaybe =<< Queue.readTimeout @[Entry _] (Seconds 5))

assertItems ::
  Members [Hedgehog IO, Queue [Entry i]] r =>
  [Text] ->
  Sem r ()
assertItems i =
  assertEq i =<< currentItems
