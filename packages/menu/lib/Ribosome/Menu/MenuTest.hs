module Ribosome.Menu.MenuTest where

import Conc (Consume, Gates, timeout_, withAsync_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Lens.Micro.Extras (view)
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
import Ribosome.Menu.Class.MenuState (MenuState (Item))
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Query, Rendered), QueryEvent (Refined))
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItemLines)
import Ribosome.Menu.Data.WindowConfig (WindowConfig (WindowConfig))
import Ribosome.Menu.Effect.Menu (MenuEngine, MenuEngineStack, Menus, bundleMenuEngine, waitPrompt)
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import qualified Ribosome.Menu.Effect.MenuTest as MenuTest
import Ribosome.Menu.Effect.MenuTest (MenuTest, waitEvents)
import Ribosome.Menu.Interpreter.Menu (MenuLoopDeps, interpretMenuDeps, interpretMenus)
import Ribosome.Menu.Interpreter.MenuFilter (interpretFilter)
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
  MenuTestWith s result ++ [Menus s, ReportLog]

type MenuTestStack i result =
  Reader (SerialT IO (MenuItem i)) : MenuLoopDeps ++ MenuTestResources i result

data TestMenuConfig =
  TestMenuConfig {
    nativePrompt :: Maybe Bool,
    initialItems :: Maybe Bool,
    prompt :: Maybe PromptConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

confDefault :: Lens' TestMenuConfig (Maybe a) -> a -> TestMenuConfig -> TestMenuConfig
confDefault attr a =
  attr %~ (<|> Just a)

confSet :: Lens' TestMenuConfig (Maybe a) -> a -> TestMenuConfig -> TestMenuConfig
confSet attr a =
  attr .~ Just a

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
  Members [MenuTest i result, AtomicState WaitState, Resource, Embed IO] r =>
  Sem r a ->
  Sem r a
withEventLog =
  flip onException do
    MenuTest.quit
    -- want this to always be printed, but using Log.crit sends it to nvim as well, and adding StderrLog just for this
    -- seems overblown
    embed . putStrLn . toString . format =<< atomicGets (view #events)
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
  TestMenuConfig ->
  (MappingLhs -> Maybe (MenuWidget s (MenuTestLoop s result ++ r) result)) ->
  InterpretersFor (MenuTestLoop s result) r
menuTestLoop conf mappings sem = do
  TestTimeout timeout <- ask
  interpretMenuTest nativePrompt pconf $ insertAt @2 $ withEventLog do
    withAsync_ (Sync.putWait timeout =<< menuLoop' mappings) do
      timeout_ (fail "prompt didn't start") timeout waitPrompt
      waitEvents "initial prompt update and renders" initialEvents
      sem
  where
    initialEvents = [Query Refined, Rendered] <> if initialItems then [Rendered] else []
    nativePrompt = fromMaybe False conf.nativePrompt
    initialItems = fromMaybe True conf.initialItems
    pconf = fromMaybe def conf.prompt

testMenuWith ::
  ∀ result s r .
  Show (Item s) =>
  Members MenuTestIOStack r =>
  Member (EventConsumer MenuEvent) r =>
  Members (MenuTestResources (Item s) result) r =>
  Members [Reader (SerialT IO (MenuItem (Item s))), Menus s, ReportLog] r =>
  TestMenuConfig ->
  s ->
  (MappingLhs -> Maybe (MenuWidget s (MenuTestWith s result ++ r) result)) ->
  InterpretersFor (MenuTestWith s result) r
testMenuWith conf initial mappings sem = do
  items <- ask
  runMenu items initial $ bundleMenuEngine (menuTestLoop conf mappings sem)

testMenu ::
  ∀ result s r .
  MenuState s =>
  Members MenuLoopDeps r =>
  Members MenuTestIOStack r =>
  Member (Reader (SerialT IO (MenuItem (Item s)))) r =>
  Members (MenuTestDeps s result) r =>
  TestMenuConfig ->
  s ->
  Mappings s (MenuTestEffects s result ++ r) result ->
  InterpretersFor (MenuTestEffects s result) r
testMenu conf initial maps =
  interpretReportLogLog .
  interpretMenuUiNvimNull .
  interpretFilter .
  interpretMenus .
  resumeReportFail .
  addMenuUi () .
  raiseUnder .
  raiseUnder3 .
  testMenuWith (confDefault #nativePrompt False conf) initial (lookupMapping maps)

testStaticMenu ::
  ∀ result s r .
  MenuState s =>
  Members MenuTestIOStack r =>
  Members (MenuTestDeps s result) r =>
  [MenuItem (Item s)] ->
  TestMenuConfig ->
  s ->
  Mappings s (MenuTestEffects s result ++ MenuTestStack (Item s) result ++ r) result ->
  InterpretersFor (MenuTestEffects s result ++ MenuTestStack (Item s) result) r
testStaticMenu items conf initial maps =
  runStaticTestMenu @(Item s) pconf items .
  testMenu @_ @s (confDefault #initialItems (not (null items)) conf') initial maps
  where
    pconf = fromMaybe def conf'.prompt
    conf' = confDefault #prompt def conf

testNvimMenu ::
  ∀ result s r .
  MenuState s =>
  Member (Reader (SerialT IO (MenuItem (Item s)))) r =>
  Members MenuLoopDeps r =>
  Members MenuTestIOStack r =>
  Members (MenuTestDeps s result) r =>
  Member (EventConsumer Event) r =>
  Members [Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  TestMenuConfig ->
  s ->
  ScratchOptions ->
  Mappings s (MenuTestEffects s result ++ r) result ->
  InterpretersFor (MenuTestEffects s result) r
testNvimMenu conf initial options maps =
  interpretReportLogLog .
  interpretMenuUiWindow .
  interpretFilter .
  interpretMenus .
  resumeReportFail .
  addMenuUi (WindowConfig pconf options (Just def) (Map.keys maps)) .
  raiseUnder .
  raiseUnder3 .
  testMenuWith conf' initial (lookupMapping maps)
  where
    pconf = fromMaybe def conf'.prompt
    conf' = confDefault #prompt def conf

testStaticNvimMenu ::
  ∀ result s r .
  MenuState s =>
  Members MenuTestIOStack r =>
  Member (EventConsumer Event) r =>
  Members [Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  [MenuItem (Item s)] ->
  TestMenuConfig ->
  s ->
  ScratchOptions ->
  Mappings s (MenuTestEffects s result ++ MenuTestStack (Item s) result ++ r) result ->
  InterpretersFor (MenuTestEffects s result ++ MenuTestStack (Item s) result) r
testStaticNvimMenu items conf initial options maps =
  runStaticTestMenu @(Item s) pconf items .
  testNvimMenu @_ @s conf' initial options maps
  where
    pconf = fromMaybe def conf'.prompt
    conf' = confDefault #prompt def conf

testStaticNvimMenuSimple ::
  ∀ result s r .
  Item s ~ () =>
  MenuState s =>
  Members MenuTestIOStack r =>
  Members [EventConsumer Event, MenuFilter] r =>
  Members [Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  [NonEmpty Text] ->
  TestMenuConfig ->
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
  Members [EventConsumer Event, MenuFilter] r =>
  Members [Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  SerialT IO (MenuItem (Item s)) ->
  TestMenuConfig ->
  s ->
  ScratchOptions ->
  Mappings s (MenuTestEffects s result ++ MenuTestStack (Item s) result ++ r) result ->
  InterpretersFor (MenuTestEffects s result ++ MenuTestStack (Item s) result) r
testNativeMenu' items conf initial options maps =
  interpretMenuTestResources (Seconds 5) pconf .
  interpretMenuDeps .
  runReader items .
  testNvimMenu @_ @s conf' initial options maps
  where
    pconf = fromMaybe def conf'.prompt
    conf' = confDefault #nativePrompt True (confDefault #prompt def conf)

testNativeMenu ::
  ∀ result s r .
  MenuState s =>
  Members MenuTestIOStack r =>
  Members [EventConsumer Event, Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  SerialT IO (MenuItem (Item s)) ->
  TestMenuConfig ->
  s ->
  ScratchOptions ->
  Mappings s (MenuTestEffects s result ++ MenuTestStack (Item s) result |> MenuFilter ++ r) result ->
  InterpretersFor (MenuTestEffects s result ++ MenuTestStack (Item s) result |> MenuFilter) r
testNativeMenu items conf initial options maps =
  interpretFilter .
  testNativeMenu' items conf initial options maps
