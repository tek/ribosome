module Ribosome.Menu.MenuTest where

import Conc (Restoration, timeout_, withAsync_)
import qualified Streamly.Prelude as Stream
import qualified Sync
import Time (Seconds (Seconds))

import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer)
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer, withMenuRenderer)
import Ribosome.Menu.Effect.MenuState (MenuState)
import Ribosome.Menu.Effect.MenuStream (MenuStream)
import Ribosome.Menu.Effect.MenuTest (MenuTest, waitItemsDone)
import Ribosome.Menu.Effect.PromptControl (PromptControl, waitPromptListening)
import Ribosome.Menu.Effect.PromptEvents (PromptEvents)
import Ribosome.Menu.Effect.PromptInput (PromptInput)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer)
import Ribosome.Menu.Effect.PromptState (PromptState)
import Ribosome.Menu.Effect.PromptStream (PromptStream)
import Ribosome.Menu.Interpreter.MenuConsumer (Mappings, withMappings)
import Ribosome.Menu.Interpreter.MenuRenderer (interpretMenuRendererNull)
import Ribosome.Menu.Interpreter.MenuState (MenuIOStack, interpretMenuFinal)
import Ribosome.Menu.Interpreter.MenuTest (
  MenuTestResources,
  TestTimeout (TestTimeout),
  interpretMenuTest,
  interpretMenuTestResources,
  )
import Ribosome.Menu.Interpreter.PromptRenderer (interpretPromptRendererNull)
import Ribosome.Menu.Main (menuMain)
import Ribosome.Menu.Nvim (interpretNvimMenu)
import Ribosome.Menu.Prompt.Data.PromptFlag (PromptFlag)
import Ribosome.Menu.Stream.Util (queueStream)

type MenuTestDeps i result =
  [
    MenuConsumer result,
    Reader (MenuConfig i),
    MenuState i,
    MenuStream i,
    MenuFilter,
    PromptStream,
    PromptState,
    PromptEvents,
    PromptControl
  ] ++ MenuTestResources i result

type MenuTestIOStack =
  [
    Log,
    Fail,
    Mask Restoration,
    Resource,
    Race,
    Async,
    Embed IO,
    Final IO
  ]

type MenuTestEffects i result =
  [
    PromptInput,
    MenuTest i result,
    PromptRenderer
  ]

type MenuTestStack i result =
  MenuIOStack i ++ MenuTestResources i result

runTestMenuWith ::
  TimeUnit u =>
  Members MenuTestIOStack r =>
  u ->
  [PromptFlag] ->
  Bool ->
  InterpretersFor (MenuTestStack i a) r
runTestMenuWith timeout flags monotonic sem =
  interpretMenuTestResources timeout do
    items <- queueStream
    interpretMenuFinal (MenuConfig items) flags monotonic sem

runTestMenu ::
  Members MenuTestIOStack r =>
  [PromptFlag] ->
  Mappings (MenuTestStack i a ++ r) a ->
  InterpretersFor (MenuConsumer a : MenuTestStack i a) r
runTestMenu flags maps =
  runTestMenuWith (Seconds 5) flags True . withMappings maps

runStaticTestMenuWith ::
  TimeUnit u =>
  Members MenuTestIOStack r =>
  u ->
  [MenuItem i] ->
  [PromptFlag] ->
  Bool ->
  InterpretersFor (MenuTestStack i a) r
runStaticTestMenuWith timeout items flags monotonic sem =
  interpretMenuTestResources timeout do
    interpretMenuFinal (MenuConfig (Stream.fromList items)) flags monotonic sem

runStaticTestMenu ::
  Members MenuTestIOStack r =>
  [MenuItem i] ->
  [PromptFlag] ->
  Mappings (MenuTestStack i a ++ r) a ->
  InterpretersFor (MenuConsumer a : MenuTestStack i a) r
runStaticTestMenu items flags maps =
  runStaticTestMenuWith (Seconds 5) items flags True . withMappings maps

testMenuRender ::
  Show i =>
  Show result =>
  Members MenuTestIOStack r =>
  Members (MenuTestDeps i result) r =>
  Members [MenuRenderer i, Scoped pres PromptRenderer] r =>
  InterpretersFor (MenuTestEffects i result) r
testMenuRender sem = do
  TestTimeout timeout <- ask
  interpretMenuTest $ withAsync_ (Sync.putWait timeout =<< menuMain) do
    timeout_ (fail "prompt didn't start") timeout waitPromptListening
    sem

testMenu ::
  Show i =>
  Show result =>
  Members MenuTestIOStack r =>
  Members (MenuTestDeps i result) r =>
  InterpretersFor (MenuTestEffects i result) r
testMenu =
  interpretMenuRendererNull . interpretPromptRendererNull . testMenuRender . insertAt @3

testNvimMenu ::
  ∀ i result r .
  Show i =>
  Show result =>
  Members MenuTestIOStack r =>
  Members (MenuTestDeps i result) r =>
  Members [Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch] r =>
  ScratchOptions ->
  InterpretersFor (MenuTestEffects i result) r
testNvimMenu scratch sem = do
  interpretNvimMenu scratch $ withMenuRenderer do
    testMenuRender (insertAt @3 sem)

testStaticMenuRender ::
  ∀ i result pres r .
  Show i =>
  Show result =>
  Members MenuTestIOStack r =>
  Members (MenuTestDeps i result) r =>
  Members [MenuRenderer i, Scoped pres PromptRenderer] r =>
  InterpretersFor (MenuTestEffects i result) r
testStaticMenuRender sem =
  testMenuRender do
    TestTimeout timeout <- ask
    timeout_ (fail "items didn't terminate") timeout waitItemsDone
    sem

testStaticMenu ::
  ∀ i result r .
  Show i =>
  Show result =>
  Members MenuTestIOStack r =>
  Members (MenuTestDeps i result) r =>
  InterpretersFor (MenuTestEffects i result) r
testStaticMenu =
  interpretMenuRendererNull . interpretPromptRendererNull . testStaticMenuRender . insertAt @3

testStaticNvimMenu ::
  ∀ i result r .
  Show i =>
  Show result =>
  Members MenuTestIOStack r =>
  Members (MenuTestDeps i result) r =>
  Members [Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch] r =>
  ScratchOptions ->
  InterpretersFor (MenuTestEffects i result) r
testStaticNvimMenu scratch sem = do
  interpretNvimMenu scratch $ withMenuRenderer do
    testStaticMenuRender (insertAt @3 sem)
