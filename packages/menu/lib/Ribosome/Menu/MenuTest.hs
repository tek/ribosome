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
import Ribosome.Menu.Class.FilterEnum (FilterEnum)
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Query))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.WindowConfig (WindowConfig (WindowConfig))
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import Ribosome.Menu.Effect.MenuLoop (MenuLoop, MenuLoops, waitPrompt)
import Ribosome.Menu.Effect.MenuTest (MenuTest, waitEventPred)
import Ribosome.Menu.Effect.MenuUi (MenuUi, withMenuUi)
import Ribosome.Menu.Interpreter.MenuLoop (MenuLoopDeps, interpretMenuLoopDeps, interpretMenuLoops)
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

type MenuTestDeps i result =
  [
    GatesIO,
    Stop RpcError
  ] ++ MenuTestResources i result

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

type MenuRenderEffects f i result =
  [MenuLoop f i, MenuTest i result, Consume MenuEvent] ++ MenuLoops f i : MenuLoopDeps

type MenuTestEffects f i result =
  MenuRenderEffects f i result |> MenuUi

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
  Members MenuTestIOStack r =>
  PromptConfig ->
  InterpretersFor (MenuTestStack i result) r
runTestMenu =
  runTestMenuWith (Seconds 5)

runStaticTestMenuWith ::
  ∀ u i result r .
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
  Members MenuTestIOStack r =>
  PromptConfig ->
  [MenuItem i] ->
  InterpretersFor (MenuTestStack i a) r
runStaticTestMenu =
  runStaticTestMenuWith (Seconds 5)

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
  Show i =>
  FilterEnum f =>
  Members MenuTestIOStack r =>
  Members (MenuTestResources i result) r =>
  Members [MenuFilter f, Reader (SerialT IO (MenuItem i)), MenuUi, Stop RpcError] r =>
  PromptConfig ->
  f ->
  (MappingLhs -> Maybe (MenuWidget f i (MenuRenderEffects f i result ++ r) result)) ->
  InterpretersFor (MenuRenderEffects f i result) r
testMenuRender pconf initialFilter mappings sem = do
  TestTimeout timeout <- ask
  items <- ask
  interpretMenuLoopDeps $ interpretMenuLoops $ interpretMenuTest pconf $ runMenu items initialFilter $ withEventLog do
    withAsync_ (Sync.putWait timeout =<< menuLoop mappings) do
      timeout_ (fail "prompt didn't start") timeout waitPrompt
      waitEventPred "initial prompt update" \case
        Query _ -> True
        _ -> False
      sem

testMenu ::
  Show i =>
  FilterEnum f =>
  Members MenuTestIOStack r =>
  Members [MenuFilter f, Reader (SerialT IO (MenuItem i))] r =>
  Members (MenuTestDeps i result) r =>
  PromptConfig ->
  f ->
  Mappings f i (MenuRenderEffects f i result ++ MenuUi : r) result ->
  InterpretersFor (MenuTestEffects f i result) r
testMenu pconf initialFilter maps =
  interpretMenuUiNvimNull .
  resumeReportFail .
  withMenuUi def .
  raiseUnder2 .
  testMenuRender pconf initialFilter (lookupMapping maps)

testStaticMenu ::
  Show i =>
  FilterEnum f =>
  Members MenuTestIOStack r =>
  Member (MenuFilter f) r =>
  Members (MenuTestDeps i result) r =>
  [MenuItem i] ->
  PromptConfig ->
  f ->
  Mappings f i (MenuRenderEffects f i result ++ MenuUi : MenuTestStack i result ++ r) result ->
  InterpretersFor (MenuTestEffects f i result ++ MenuTestStack i result) r
testStaticMenu items pconf initialFilter maps =
  runStaticTestMenu pconf items .
  testMenu pconf initialFilter maps

testNvimMenu ::
  ∀ i result f r .
  Show i =>
  FilterEnum f =>
  Member (Reader (SerialT IO (MenuItem i))) r =>
  Members MenuTestIOStack r =>
  Members (MenuTestDeps i result) r =>
  Members [ChanConsumer Event, MenuFilter f, Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError] r =>
  PromptConfig ->
  f ->
  ScratchOptions ->
  Mappings f i (MenuRenderEffects f i result ++ MenuUi : r) result ->
  InterpretersFor (MenuTestEffects f i result) r
testNvimMenu pconf initialFilter options maps =
  interpretMenuUiWindow .
  resumeReportFail .
  withMenuUi (WindowConfig pconf options (Just def) (Map.keys maps)) .
  raiseUnder2 .
  testMenuRender pconf initialFilter (lookupMapping maps)

testStaticNvimMenu ::
  ∀ i result f r .
  Show i =>
  FilterEnum f =>
  Members MenuTestIOStack r =>
  Members [ChanConsumer Event, MenuFilter f, Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  [MenuItem i] ->
  PromptConfig ->
  f ->
  ScratchOptions ->
  Mappings f i (MenuRenderEffects f i result ++ MenuUi : MenuTestStack i result ++ r) result ->
  InterpretersFor (MenuTestEffects f i result ++ MenuTestStack i result) r
testStaticNvimMenu items pconf initialFilter options maps =
  runStaticTestMenu pconf items .
  testNvimMenu pconf initialFilter options maps
