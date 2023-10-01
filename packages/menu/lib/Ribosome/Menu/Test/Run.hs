module Ribosome.Menu.Test.Run where

import Conc (Consume, Gates, timeout_, withAsync_)
import qualified Data.Text as Text
import Exon (exon)
import Lens.Micro.Extras (view)
import qualified Log
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (SerialT)
import qualified Sync
import Time (Seconds (Seconds))

import Ribosome.Data.Mapping (MappingSpec)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.Event (Event)
import Ribosome.Host.Data.Report (ReportLog, resumeReportFail)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpreter.Log (interpretReportLogLog)
import qualified Ribosome.Menu.App
import Ribosome.Menu.App (MenuApp, PromptApp, hoistPromptApp)
import Ribosome.Menu.Class.MenuState (MenuState (Item))
import Ribosome.Menu.Data.MenuConfig (MenuConfig)
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Query, Rendered), QueryEvent (Refined))
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItemLines)
import Ribosome.Menu.Data.WindowConfig (WindowConfig (WindowConfig))
import Ribosome.Menu.Effect.Menu (MenuLoop, Menus, UiMenus, waitPrompt)
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import qualified Ribosome.Menu.Effect.MenuTest as MenuTest
import Ribosome.Menu.Effect.MenuTest (MenuTest, waitEvents)
import Ribosome.Menu.Effect.MenuUi (ScopedMenuUi)
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
import Ribosome.Menu.Loop (menuAppWith, menuLoop, menuParams, withUi)
import Ribosome.Menu.Prompt.Data.Prompt (PromptModes (StartInsert), PromptState)
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

type TestLoop s result =
  [MenuTest (Item s) result, Consume MenuEvent]

type MenuAppTest s result =
  TestLoop s result ++ MenuLoop s ++ [EventConsumer MenuEvent, Reader MenuConfig]

type MenuTestStack i result =
  Reader (SerialT IO (MenuItem i)) : MenuLoopDeps ++ MenuTestResources i result

data TestMenuConfig =
  TestMenuConfig {
    nativePrompt :: Maybe Bool,
    initialItems :: Maybe Bool,
    prompt :: Maybe PromptState,
    addBuiltin :: Maybe Bool,
    addDefault :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

type MenuTestAppStack s result =
  MenuTest (Item s) result : MenuLoop s

type PromptTestApp s r result =
  PromptApp s (MenuTestAppStack s result ++ r) result

type MenuTestApp s r result =
  MenuApp s (MenuTestAppStack s result ++ r) result

confDefault :: Lens' TestMenuConfig (Maybe a) -> a -> TestMenuConfig -> TestMenuConfig
confDefault attr a =
  attr %~ (<|> Just a)

confSet :: Lens' TestMenuConfig (Maybe a) -> a -> TestMenuConfig -> TestMenuConfig
confSet attr a =
  attr .~ Just a

noItems :: TestMenuConfig -> TestMenuConfig
noItems = confSet #initialItems False

noItemsConf :: TestMenuConfig
noItemsConf = noItems def

startInsert :: TestMenuConfig -> TestMenuConfig
startInsert =
  confSet #prompt (def & #modes .~ StartInsert)

startInsertConf :: TestMenuConfig
startInsertConf = startInsert def

runTestMenuWith ::
  ∀ u i result r .
  TimeUnit u =>
  Members MenuTestIOStack r =>
  u ->
  InterpretersFor (MenuTestStack i result) r
runTestMenuWith timeout sem =
  interpretMenuTestResources timeout $ interpretMenuDeps do
    items <- queueStream
    runReader items sem

runTestMenu ::
  ∀ result i r .
  Members MenuTestIOStack r =>
  InterpretersFor (MenuTestStack i result) r
runTestMenu =
  runTestMenuWith (Seconds 5)

runStaticTestMenuWith ::
  ∀ result i u r .
  TimeUnit u =>
  Members MenuTestIOStack r =>
  u ->
  [MenuItem i] ->
  InterpretersFor (MenuTestStack i result) r
runStaticTestMenuWith timeout items =
  interpretMenuTestResources timeout .
  interpretMenuDeps .
  runReader (Stream.fromList items)

runStaticTestMenu ::
  ∀ i a r .
  Members MenuTestIOStack r =>
  [MenuItem i] ->
  InterpretersFor (MenuTestStack i a) r
runStaticTestMenu =
  runStaticTestMenuWith (Seconds 5)

withEventLog ::
  Members [MenuTest i result, AtomicState WaitState, Resource, Embed IO] r =>
  TestMenuConfig ->
  Sem r a ->
  Sem r a
withEventLog conf =
  flip onException do
    MenuTest.quit
    -- want this to always be printed, but using Log.crit sends it to nvim as well, and adding StderrLog just for this
    -- seems overblown
    events <- atomicGets (view #events)
    embed do
      putStrLn (toString (format events))
      putStrLn (show conf)
  where
    format evs =
      Text.unlines ("wait events:" : (("  " <>) . show <$> reverse evs))

menuTestLoop ::
  ∀ result s r .
  Show (Item s) =>
  Members MenuTestIOStack r =>
  Members (MenuLoop s) r =>
  Members (MenuTestResources (Item s) result) r =>
  Members [ReportLog, EventConsumer MenuEvent] r =>
  TestMenuConfig ->
  PromptApp s (TestLoop s result ++ r) result ->
  InterpretersFor (TestLoop s result) r
menuTestLoop conf app sem = do
  TestTimeout timeout <- ask
  Log.debug [exon|Test config: #{show conf}|]
  interpretMenuTest nativePrompt $ withEventLog conf do
    withAsync_ (Sync.putWait timeout =<< menuLoop app) do
      timeout_ (fail "prompt didn't start") timeout waitPrompt
      waitEvents "initial prompt update and renders" initialEvents
      insertAt @2 sem
  where
    initialEvents = [Query Refined, Rendered] <> if initialItems then [Rendered] else []
    nativePrompt = fromMaybe False conf.nativePrompt
    initialItems = fromMaybe True conf.initialItems

promptTestApp ::
  ∀ result s r .
  Show (Item s) =>
  Members MenuTestIOStack r =>
  Member (EventConsumer MenuEvent) r =>
  Members (MenuTestResources (Item s) result) r =>
  Members [Reader (SerialT IO (MenuItem (Item s))), Menus s, ReportLog] r =>
  TestMenuConfig ->
  s ->
  PromptTestApp s r result ->
  InterpretersFor (TestLoop s result ++ MenuLoop s) r
promptTestApp conf initial app sem = do
  items <- ask
  menuParams items initial do
    menuTestLoop conf (hoistPromptApp (insertAt @3) app) (insertAt @5 sem)

type TestMenuEffects ui s result =
  [
    UiMenus ui s,
    UiMenus ui s !! RpcError,
    MenuFilter,
    ReportLog
  ]

interpretTestMenuLocal ::
  ∀ ui s result r .
  MenuState s =>
  Member (ScopedMenuUi ui) r =>
  Members MenuLoopDeps r =>
  Members MenuTestIOStack r =>
  InterpretersFor (TestMenuEffects ui s result) r
interpretTestMenuLocal =
  interpretReportLogLog .
  interpretFilter .
  interpretMenus .
  resumeReportFail

testMenu ::
  ∀ ui result s r .
  MenuState s =>
  Members MenuLoopDeps r =>
  Members MenuTestIOStack r =>
  Member (ScopedMenuUi ui) r =>
  Member (Reader (SerialT IO (MenuItem (Item s)))) r =>
  Members (MenuTestDeps s result) r =>
  ([MappingSpec] -> ui) ->
  TestMenuConfig ->
  s ->
  MenuTestApp s r result ->
  InterpretersFor (TestLoop s result ++ MenuLoop s) r
testMenu ui conf initial app sem =
  interpretTestMenuLocal do
    menuAppWith addBuiltin addDefault prompt app \ papp ->
      withUi (ui papp.mappings) do
        promptTestApp conf' initial (hoistPromptApp (insertAt @6) papp) $ insertAt @5 sem
  where
    -- TODO should this be in headlessTestMenu?
    conf' = confDefault #nativePrompt False conf
    addBuiltin = fromMaybe True conf.addBuiltin
    addDefault = fromMaybe True conf.addDefault
    prompt = fromMaybe def conf.prompt

headlessTestMenu ::
  ∀ result s r .
  MenuState s =>
  Members MenuLoopDeps r =>
  Members MenuTestIOStack r =>
  Member (Reader (SerialT IO (MenuItem (Item s)))) r =>
  Members (MenuTestDeps s result) r =>
  TestMenuConfig ->
  s ->
  MenuTestApp s r result ->
  InterpretersFor (TestLoop s result ++ MenuLoop s) r
headlessTestMenu conf initial app =
  interpretMenuUiNvimNull .
  testMenu (const ()) conf initial (insertAt @6 <$> app) .
  insertAt @5

testStaticMenu ::
  ∀ result s r .
  MenuState s =>
  Member (Stop RpcError) r =>
  Members MenuTestIOStack r =>
  [MenuItem (Item s)] ->
  TestMenuConfig ->
  s ->
  MenuTestApp s r result ->
  InterpretersFor (MenuAppTest s result) r
testStaticMenu items conf initial app =
  runStaticTestMenu @(Item s) items .
  subsume .
  subsume .
  headlessTestMenu (confDefault #initialItems (not (null items)) conf) initial (insertAt @6 <$> app) .
  insertAt @7

nvimTestMenu ::
  ∀ result s r .
  MenuState s =>
  Member (Reader (SerialT IO (MenuItem (Item s)))) r =>
  Members MenuLoopDeps r =>
  Members MenuTestIOStack r =>
  Members (MenuTestDeps s result) r =>
  Member (EventConsumer Event) r =>
  Members [Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  TestMenuConfig ->
  s ->
  ScratchOptions ->
  MenuTestApp s r result ->
  InterpretersFor (TestLoop s result ++ MenuLoop s) r
nvimTestMenu conf initial options app =
  interpretMenuUiWindow .
  testMenu (WindowConfig options (Just def)) conf' initial (insertAt @6 <$> app) .
  insertAt @5
  where
    conf' = confDefault #prompt def conf

testStaticNvimMenu ::
  ∀ result s r .
  MenuState s =>
  Members MenuTestIOStack r =>
  Members [EventConsumer Event, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  [MenuItem (Item s)] ->
  TestMenuConfig ->
  s ->
  ScratchOptions ->
  MenuTestApp s r result ->
  InterpretersFor (MenuAppTest s result) r
testStaticNvimMenu items conf initial options app =
  runStaticTestMenu @(Item s) items .
  subsume .
  subsume .
  nvimTestMenu conf' initial options (insertAt @6 <$> app) .
  insertAt @7
  where
    conf' = confDefault #prompt def conf

testStaticNvimMenuSimple ::
  ∀ result s r .
  Item s ~ () =>
  MenuState s =>
  Members MenuTestIOStack r =>
  Members [EventConsumer Event, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  [NonEmpty Text] ->
  TestMenuConfig ->
  s ->
  ScratchOptions ->
  MenuTestApp s r result ->
  InterpretersFor (MenuAppTest s result) r
testStaticNvimMenuSimple items =
  testStaticNvimMenu (simpleMenuItemLines () <$> items)

testNativeMenu ::
  ∀ result s r .
  MenuState s =>
  Members MenuTestIOStack r =>
  Members [EventConsumer Event, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  SerialT IO (MenuItem (Item s)) ->
  TestMenuConfig ->
  s ->
  ScratchOptions ->
  MenuTestApp s r result ->
  InterpretersFor (MenuAppTest s result) r
testNativeMenu items conf initial options app =
  interpretMenuTestResources (Seconds 5) .
  interpretMenuDeps .
  runReader items .
  subsume .
  subsume .
  nvimTestMenu conf' initial options (insertAt @6 <$> app) .
  insertAt @7
  where
    conf' = confDefault #nativePrompt True (confDefault #prompt def conf)
