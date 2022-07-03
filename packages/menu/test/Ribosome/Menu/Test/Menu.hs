module Ribosome.Menu.Test.Menu where

import Conc (Restoration, interpretQueueTBM, resultToMaybe)
import Control.Lens (use, (^.))
import Polysemy.Log (interpretLogNull)
import Polysemy.Test (Hedgehog, TestError (TestError), assertEq, assertJust, evalMaybe)
import qualified Queue
import Time (Seconds (Seconds))

import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Action (menuIgnore, menuQuit)
import Ribosome.Menu.Combinators (sortedEntries)
import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entry)
import Ribosome.Menu.Data.MenuAction (MenuAction)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuState (
  CursorLock,
  MenuStack,
  MenuStateEffects,
  menuRead,
  semState,
  )
import Ribosome.Menu.Data.NvimMenuConfig (NvimMenuConfig)
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer (MenuConsumerEvent))
import qualified Ribosome.Menu.Effect.MenuRenderer as MenuRenderer
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer)
import Ribosome.Menu.Effect.MenuTest (sendStaticItems)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer)
import Ribosome.Menu.Filters (fuzzyMonotonic)
import Ribosome.Menu.Interpreter.MenuTest (MenuTestEffects, menuTest, staticNvimMenuTest)
import Ribosome.Menu.Interpreter.PromptRenderer (interpretPromptRendererNull)
import Ribosome.Menu.Main (interpretMenu)
import Ribosome.Menu.Prompt (PromptFlag (StartInsert), PromptListening)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

enqueueItems ::
  Members [Hedgehog IO, Queue [Entry i]] r =>
  InterpreterFor (MenuRenderer i) r
enqueueItems =
  interpret \case
    MenuRenderer.MenuRender menu -> do
      evalMaybe . resultToMaybe =<< Queue.writeTimeout (Seconds 5) (menu ^. sortedEntries)
    MenuRenderer.MenuRenderQuit ->
      unit

enqueuePrompt ::
  ∀ i r .
  Members (MenuStateEffects i) r =>
  Members [Hedgehog IO, Queue Prompt, Sync CursorLock, Resource, Embed IO] r =>
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
    enq =
      menuRead do
        Queue.write =<< semState (use #prompt)
        raise menuIgnore

type MenuTestStack i =
  [
    Scoped () PromptRenderer,
    MenuRenderer i,
    Sync PromptListening
  ] ++ MenuStack i ++ [
    Log,
    Queue [Entry i],
    Queue Prompt
  ]

runMenuTestStack ::
  ∀ i r .
  Members [Hedgehog IO, Resource, Race, Mask Restoration, Embed IO, Final IO] r =>
  InterpretersFor (MenuTestStack i) r
runMenuTestStack =
  interpretQueueTBM @Prompt 64 .
  interpretQueueTBM @[Entry i] 64 .
  interpretLogNull .
  interpretMenu .
  enqueueItems .
  interpretPromptRendererNull

type PromptTest i =
  MenuTestEffects i () ++ MenuConsumer () : MenuTestStack i

promptTest ::
  Show i =>
  Members [Hedgehog IO, Error TestError, Resource, Race, Mask Restoration, Async, Embed IO, Final IO] r =>
  InterpretersFor (PromptTest i) r
promptTest =
  runMenuTestStack .
  enqueuePrompt .
  menuTest [StartInsert] fuzzyMonotonic (Seconds 5) (throw . TestError)

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

menuTestDef ::
  ∀ result i pres r .
  Show i =>
  Show result =>
  Members (MenuStack i) r =>
  Members [MenuConsumer result, MenuRenderer i, Scoped pres PromptRenderer] r =>
  Members [Sync PromptListening, Error TestError, Log, Mask Restoration, Race, Resource, Async, Embed IO, Final IO] r =>
  InterpretersFor (MenuTestEffects i result) r
menuTestDef =
  menuTest [StartInsert] fuzzyMonotonic (Seconds 5) (throw . TestError)

staticMenuTestDef ::
  ∀ result i pres r .
  Show i =>
  Show result =>
  Members (MenuStack i) r =>
  Members [MenuConsumer result, MenuRenderer i, Scoped pres PromptRenderer] r =>
  Members [Sync PromptListening, Error TestError, Log, Mask Restoration, Race, Resource, Async, Embed IO, Final IO] r =>
  [MenuItem i] ->
  InterpretersFor (MenuTestEffects i result) r
staticMenuTestDef items test =
  menuTestDef do
    sendStaticItems items
    test

staticNvimMenuTestDef ::
  ∀ result i r .
  Show i =>
  Show result =>
  Members (MenuStack i) r =>
  Members [MenuConsumer result, Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch] r =>
  Members [Sync PromptListening, Error TestError, Log, Mask Restoration, Race, Resource, Async, Embed IO, Final IO] r =>
  NvimMenuConfig i ->
  InterpretersFor (MenuTestEffects i result) r
staticNvimMenuTestDef conf =
  staticNvimMenuTest conf (Seconds 5) (throw . TestError)
