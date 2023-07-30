module Ribosome.Menu.MenuTest where

import Conc (Consume, Gates, timeout_, withAsync_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Lens.Micro.Extras (view)
import qualified Log
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (SerialT)
import qualified Sync
import Time (Seconds (Seconds))

import Ribosome.Data.Mapping (MappingLhs)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.Event (Event)
import Ribosome.Host.Data.Report (ReportLog, resumeReportFail)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpret (type (|>))
import Ribosome.Host.Interpreter.Log (interpretReportLogLog)
import Ribosome.Menu.Action (MenuWidget)
import Ribosome.Menu.Class.MenuState (Filter, MenuState (Item))
import qualified Ribosome.Menu.Data.Filter as Data
import Ribosome.Menu.Data.FilterMode (FilterMode)
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Query))
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItemLines)
import Ribosome.Menu.Data.WindowConfig (WindowConfig (WindowConfig))
import Ribosome.Menu.Effect.Menu (MenuEngine, MenuEngineStack, Menus, bundleMenuEngine, waitPrompt)
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import Ribosome.Menu.Effect.MenuTest (MenuTest, waitEventPred)
import Ribosome.Menu.Interpreter.Menu (MenuLoopDeps, interpretMenuDeps, interpretMenus)
import Ribosome.Menu.Interpreter.MenuFilter (defaultFilter)
import Ribosome.Menu.Interpreter.MenuTest (
  MenuTestResources,
  TestTimeout (TestTimeout),
  WaitState,
  interpretMenuTest,
  interpretMenuTestResources,
  )
import Ribosome.Menu.Interpreter.MenuUi (interpretMenuUiNvimNull)
import Ribosome.Menu.Interpreter.MenuUiWindow (interpretMenuUiWindow)
import Ribosome.Menu.Loop (addMenuUi, lookupMapping, menuLoop', runMenu)
import Ribosome.Menu.Mappings (Mappings)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import Ribosome.Menu.Stream.Util (queueStream)

type MenuTestDeps s result =
  [
    Gates,
    Stop RpcError
  ] ++ MenuTestResources (Item s) result

type MenuTestIOStack =
  [
    Gates,
    Log,
    Fail,
    Mask,
    Resource,
    Race,
    Async,
    Embed IO,
    Final IO
  ]

type MenuTestLoop s result =
  [MenuTest (Item s) result, Consume MenuEvent]

type MenuTestWith s result =
  MenuTestLoop s result ++ MenuEngineStack s |> MenuEngine s

type MenuTestEffects s result =
  MenuTestWith s result ++ [Menus s, ReportLog] ++ MenuLoopDeps

type MenuTestStack i result =
  Reader (SerialT IO (MenuItem i)) : MenuLoopDeps ++ MenuTestResources i result

runTestMenuWith ::
  ∀ u i result r .
  TimeUnit u =>
  Members MenuTestIOStack r =>
  u ->
  PromptConfig ->
  InterpretersFor (MenuTestStack i result) r
runTestMenuWith timeout pconf sem =
  interpretMenuTestResources timeout pconf $ interpretMenuDeps do
    items <- queueStream
    runReader items sem

runTestMenu ::
  ∀ result i r .
  Members MenuTestIOStack r =>
  PromptConfig ->
  InterpretersFor (MenuTestStack i result) r
runTestMenu =
  runTestMenuWith @_ @i (Seconds 5)

runStaticTestMenuWith ::
  ∀ result i u r .
  TimeUnit u =>
  Members MenuTestIOStack r =>
  u ->
  PromptConfig ->
  [MenuItem i] ->
  InterpretersFor (MenuTestStack i result) r
runStaticTestMenuWith timeout pconf items =
  interpretMenuTestResources timeout pconf .
  interpretMenuDeps .
  runReader (Stream.fromList items)

runStaticTestMenu ::
  ∀ i a r .
  Members MenuTestIOStack r =>
  PromptConfig ->
  [MenuItem i] ->
  InterpretersFor (MenuTestStack i a) r
runStaticTestMenu =
  runStaticTestMenuWith @_ @i (Seconds 5)

withEventLog ::
  Members [AtomicState WaitState, Log, Resource] r =>
  Sem r a ->
  Sem r a
withEventLog =
  flip onException (Log.crit . format =<< atomicGets (view #events))
  where
    format evs =
      Text.unlines ("wait events:" : (("  " <>) . show <$> reverse evs))

menuTestLoop ::
  ∀ result s r .
  Show (Item s) =>
  Members MenuTestIOStack r =>
  Members (MenuEngineStack s) r =>
  Members (MenuTestResources (Item s) result) r =>
  Members [ReportLog, EventConsumer MenuEvent] r =>
  Bool ->
  PromptConfig ->
  (MappingLhs -> Maybe (MenuWidget s (MenuTestLoop s result ++ r) result)) ->
  InterpretersFor (MenuTestLoop s result) r
menuTestLoop nativePrompt pconf mappings sem = do
  TestTimeout timeout <- ask
  interpretMenuTest nativePrompt pconf $ insertAt @2 $ withEventLog do
    withAsync_ (Sync.putWait timeout =<< menuLoop' mappings) do
      timeout_ (fail "prompt didn't start") timeout waitPrompt
      waitEventPred "initial prompt update" \case
        Query _ -> True
        _ -> False
      sem

testMenuWith ::
  ∀ result s r .
  Show (Item s) =>
  Members MenuTestIOStack r =>
  Member (EventConsumer MenuEvent) r =>
  Members (MenuTestResources (Item s) result) r =>
  Members [Reader (SerialT IO (MenuItem (Item s))), Menus s, ReportLog] r =>
  Bool ->
  PromptConfig ->
  s ->
  (MappingLhs -> Maybe (MenuWidget s (MenuTestWith s result ++ r) result)) ->
  InterpretersFor (MenuTestWith s result) r
testMenuWith nativePrompt pconf initial mappings sem = do
  items <- ask
  runMenu items initial $ bundleMenuEngine (menuTestLoop nativePrompt pconf mappings sem)

testMenu ::
  ∀ result s r .
  MenuState s =>
  Members MenuTestIOStack r =>
  Members [MenuFilter (Filter s), Reader (SerialT IO (MenuItem (Item s)))] r =>
  Members (MenuTestDeps s result) r =>
  PromptConfig ->
  s ->
  Mappings s (MenuTestEffects s result ++ r) result ->
  InterpretersFor (MenuTestEffects s result) r
testMenu pconf initial maps =
  interpretMenuDeps .
  interpretReportLogLog .
  interpretMenuUiNvimNull .
  interpretMenus .
  resumeReportFail .
  addMenuUi () .
  raiseUnder3 .
  testMenuWith False pconf initial (lookupMapping maps)

testStaticMenu ::
  ∀ result s r .
  MenuState s =>
  Members MenuTestIOStack r =>
  Member (MenuFilter (Filter s)) r =>
  Members (MenuTestDeps s result) r =>
  [MenuItem (Item s)] ->
  PromptConfig ->
  s ->
  Mappings s (MenuTestEffects s result ++ MenuTestStack (Item s) result ++ r) result ->
  InterpretersFor (MenuTestEffects s result ++ MenuTestStack (Item s) result) r
testStaticMenu items pconf initial maps =
  runStaticTestMenu @(Item s) pconf items .
  testMenu @_ @s pconf initial maps

testNvimMenu ::
  ∀ result s r .
  MenuState s =>
  Member (Reader (SerialT IO (MenuItem (Item s)))) r =>
  Members MenuTestIOStack r =>
  Members (MenuTestDeps s result) r =>
  Members [EventConsumer Event, MenuFilter (Filter s)] r =>
  Members [Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  Bool ->
  PromptConfig ->
  s ->
  ScratchOptions ->
  Mappings s (MenuTestEffects s result ++ r) result ->
  InterpretersFor (MenuTestEffects s result) r
testNvimMenu nativePrompt pconf initial options maps =
  interpretMenuDeps .
  interpretReportLogLog .
  interpretMenuUiWindow .
  interpretMenus .
  resumeReportFail .
  addMenuUi (WindowConfig pconf options (Just def) (Map.keys maps)) .
  raiseUnder3 .
  testMenuWith nativePrompt pconf initial (lookupMapping maps)

testStaticNvimMenu ::
  ∀ result s r .
  MenuState s =>
  Members MenuTestIOStack r =>
  Members [EventConsumer Event, MenuFilter (Filter s)] r =>
  Members [Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  [MenuItem (Item s)] ->
  Bool ->
  PromptConfig ->
  s ->
  ScratchOptions ->
  Mappings s (MenuTestEffects s result ++ MenuTestStack (Item s) result ++ r) result ->
  InterpretersFor (MenuTestEffects s result ++ MenuTestStack (Item s) result) r
testStaticNvimMenu items nativePrompt pconf initial options maps =
  runStaticTestMenu @(Item s) pconf items .
  testNvimMenu @_ @s nativePrompt pconf initial options maps

testStaticNvimMenuSimple ::
  ∀ result s r .
  Item s ~ () =>
  MenuState s =>
  Members MenuTestIOStack r =>
  Members [EventConsumer Event, MenuFilter (Filter s)] r =>
  Members [Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  [NonEmpty Text] ->
  Bool ->
  PromptConfig ->
  s ->
  ScratchOptions ->
  Mappings s (MenuTestEffects s result ++ MenuTestStack (Item s) result ++ r) result ->
  InterpretersFor (MenuTestEffects s result ++ MenuTestStack (Item s) result) r
testStaticNvimMenuSimple items =
  testStaticNvimMenu (simpleMenuItemLines () <$> items)

testNativeMenu' ::
  ∀ result s r .
  MenuState s =>
  Members MenuTestIOStack r =>
  Members [EventConsumer Event, MenuFilter (Filter s)] r =>
  Members [Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  SerialT IO (MenuItem (Item s)) ->
  PromptConfig ->
  s ->
  ScratchOptions ->
  Mappings s (MenuTestEffects s result ++ MenuTestStack (Item s) result ++ r) result ->
  InterpretersFor (MenuTestEffects s result ++ MenuTestStack (Item s) result) r
testNativeMenu' items pconf initial options maps =
  interpretMenuTestResources (Seconds 5) pconf .
  interpretMenuDeps .
  runReader items .
  testNvimMenu @_ @s True pconf initial options maps

testNativeMenu ::
  ∀ result s r .
  MenuState s =>
  Filter s ~ FilterMode Data.Filter =>
  Members MenuTestIOStack r =>
  Members [EventConsumer Event, Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  SerialT IO (MenuItem (Item s)) ->
  PromptConfig ->
  s ->
  ScratchOptions ->
  Mappings s (MenuTestEffects s result ++ MenuTestStack (Item s) result |> MenuFilter (Filter s) ++ r) result ->
  InterpretersFor (MenuTestEffects s result ++ MenuTestStack (Item s) result |> MenuFilter (Filter s)) r
testNativeMenu items pconf initial options maps =
  defaultFilter .
  testNativeMenu' items pconf initial options maps
