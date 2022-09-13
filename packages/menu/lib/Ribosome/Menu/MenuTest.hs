module Ribosome.Menu.MenuTest where

import Conc (ChanConsumer, Consume, GatesIO, Restoration, timeout_, withAsync_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
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
import Ribosome.Host.Data.Report (resumeReportFail)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpret (type (|>))
import Ribosome.Menu.Action (MenuWidget)
import Ribosome.Menu.Class.MenuState (Filter, MenuState, MenuState (Item))
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Query))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.WindowConfig (WindowConfig (WindowConfig))
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import Ribosome.Menu.Effect.Menu (Menu, Menus, waitPrompt)
import Ribosome.Menu.Effect.MenuTest (MenuTest, waitEventPred)
import Ribosome.Menu.Effect.MenuUi (MenuUi, withMenuUi)
import Ribosome.Menu.Interpreter.Menu (MenuLoopDeps, interpretMenuLoopDeps, interpretMenuLoops)
import Ribosome.Menu.Interpreter.MenuTest (
  MenuTestResources,
  TestTimeout (TestTimeout),
  WaitEvent,
  interpretMenuTest,
  interpretMenuTestResources,
  )
import Ribosome.Menu.Interpreter.MenuUi (interpretMenuUiNvimNull)
import Ribosome.Menu.Interpreter.MenuUiWindow (interpretMenuUiWindow)
import Ribosome.Menu.Loop (lookupMapping, menuLoop, runMenu)
import Ribosome.Menu.Mappings (Mappings)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import Ribosome.Menu.Stream.Util (queueStream)

type MenuTestDeps s result =
  [
    GatesIO,
    Stop RpcError
  ] ++ MenuTestResources (Item s) result

type MenuTestIOStack =
  [
    GatesIO,
    Log,
    Fail,
    Mask Restoration,
    Resource,
    Race,
    Async,
    Embed IO,
    Final IO
  ]

type MenuRenderEffects s result =
  [Menu s, MenuTest (Item s) result, Consume MenuEvent] ++ Menus s : MenuLoopDeps

type MenuTestEffects s result =
  MenuRenderEffects s result |> MenuUi

type MenuTestStack i result =
  Reader (SerialT IO (MenuItem i)) : MenuTestResources i result

runTestMenuWith ::
  ∀ u i a r .
  TimeUnit u =>
  Members MenuTestIOStack r =>
  u ->
  PromptConfig ->
  InterpretersFor (MenuTestStack i a) r
runTestMenuWith timeout pconf sem =
  interpretMenuTestResources timeout pconf do
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
  Members [AtomicState [WaitEvent], Log, Resource] r =>
  Sem r a ->
  Sem r a
withEventLog =
  flip onException (Log.crit . format =<< atomicGet)
  where
    format evs =
      Text.unlines ("wait events:" : (("  " <>) . show <$> reverse evs))

testMenuRender ::
  ∀ result s r .
  MenuState s =>
  Show (Item s) =>
  Members MenuTestIOStack r =>
  Members (MenuTestResources (Item s) result) r =>
  Members [MenuFilter (Filter s), Reader (SerialT IO (MenuItem (Item s))), MenuUi, Stop RpcError] r =>
  PromptConfig ->
  s ->
  (MappingLhs -> Maybe (MenuWidget s (MenuRenderEffects s result ++ r) result)) ->
  InterpretersFor (MenuRenderEffects s result) r
testMenuRender pconf initial mappings sem = do
  TestTimeout timeout <- ask
  items <- ask
  interpretMenuLoopDeps $ interpretMenuLoops $ interpretMenuTest pconf $ runMenu items initial $ withEventLog do
    withAsync_ (Sync.putWait timeout =<< menuLoop mappings) do
      timeout_ (fail "prompt didn't start") timeout waitPrompt
      waitEventPred "initial prompt update" \case
        Query _ -> True
        _ -> False
      sem

testMenu ::
  ∀ result s r .
  MenuState s =>
  Show (Item s) =>
  Members MenuTestIOStack r =>
  Members [MenuFilter (Filter s), Reader (SerialT IO (MenuItem (Item s)))] r =>
  Members (MenuTestDeps s result) r =>
  PromptConfig ->
  s ->
  Mappings s (MenuRenderEffects s result ++ MenuUi : r) result ->
  InterpretersFor (MenuTestEffects s result) r
testMenu pconf initial maps =
  interpretMenuUiNvimNull .
  resumeReportFail .
  withMenuUi def .
  raiseUnder2 .
  testMenuRender pconf initial (lookupMapping maps)

testStaticMenu ::
  ∀ result s r .
  MenuState s =>
  Show (Item s) =>
  Members MenuTestIOStack r =>
  Member (MenuFilter (Filter s)) r =>
  Members (MenuTestDeps s result) r =>
  [MenuItem (Item s)] ->
  PromptConfig ->
  s ->
  Mappings s (MenuRenderEffects s result ++ MenuUi : MenuTestStack (Item s) result ++ r) result ->
  InterpretersFor (MenuTestEffects s result ++ MenuTestStack (Item s) result) r
testStaticMenu items pconf initial maps =
  runStaticTestMenu @(Item s) pconf items .
  testMenu @_ @s pconf initial maps

testNvimMenu ::
  ∀ result s r .
  MenuState s =>
  Show (Item s) =>
  Member (Reader (SerialT IO (MenuItem (Item s)))) r =>
  Members MenuTestIOStack r =>
  Members (MenuTestDeps s result) r =>
  Members [ChanConsumer Event, MenuFilter (Filter s), Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError] r =>
  PromptConfig ->
  s ->
  ScratchOptions ->
  Mappings s (MenuRenderEffects s result ++ MenuUi : r) result ->
  InterpretersFor (MenuTestEffects s result) r
testNvimMenu pconf initial options maps =
  interpretMenuUiWindow .
  resumeReportFail .
  withMenuUi (WindowConfig pconf options (Just def) (Map.keys maps)) .
  raiseUnder2 .
  testMenuRender pconf initial (lookupMapping maps)

testStaticNvimMenu ::
  ∀ result s r .
  MenuState s =>
  Show (Item s) =>
  Members MenuTestIOStack r =>
  Members [ChanConsumer Event, MenuFilter (Filter s), Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  [MenuItem (Item s)] ->
  PromptConfig ->
  s ->
  ScratchOptions ->
  Mappings s (MenuRenderEffects s result ++ MenuUi : MenuTestStack (Item s) result ++ r) result ->
  InterpretersFor (MenuTestEffects s result ++ MenuTestStack (Item s) result) r
testStaticNvimMenu items pconf initial options maps =
  runStaticTestMenu @(Item s) pconf items .
  testNvimMenu @_ @s pconf initial options maps
