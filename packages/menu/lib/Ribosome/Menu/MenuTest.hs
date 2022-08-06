module Ribosome.Menu.MenuTest where

import Conc (Consume, GatesIO, Restoration, timeout_, withAsync_)
import qualified Data.Map.Strict as Map
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (SerialT)
import qualified Sync
import Time (Seconds (Seconds))

import Ribosome.Data.Mapping (MappingLhs)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.Report (resumeReportFail)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpret (type (|>))
import Ribosome.Menu.Action (MenuWidget)
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Effect.MenuLoop (MenuLoop, MenuLoops, waitPrompt)
import Ribosome.Menu.Effect.MenuTest (MenuTest)
import Ribosome.Menu.Effect.MenuUi (MenuUi, NvimMenuConfig (NvimMenuConfig), withMenuUi)
import Ribosome.Menu.Interpreter.MenuLoop (MenuLoopDeps, interpretMenuLoopDeps, interpretMenuLoops)
import Ribosome.Menu.Interpreter.MenuTest (
  MenuTestResources,
  TestTimeout (TestTimeout),
  interpretMenuTest,
  interpretMenuTestResources, WaitEvent,
  )
import Ribosome.Menu.Interpreter.MenuUi (interpretMenuUiNvimNull)
import Ribosome.Menu.Interpreter.MenuUiEcho (interpretMenuUiNvimEcho)
import Ribosome.Menu.Loop (lookupMapping, menu', runMenu)
import Ribosome.Menu.Mappings (Mappings)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import Ribosome.Menu.Stream.Util (queueStream)
import qualified Log

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

type MenuRenderEffects i result =
  [MenuLoop i, MenuTest i result, Consume MenuEvent] ++ MenuLoops i : MenuLoopDeps

type MenuTestEffects i result =
  MenuRenderEffects i result |> MenuUi

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
  flip onException (Log.crit . show =<< atomicGet)

testMenuRender ::
  Show i =>
  Members MenuTestIOStack r =>
  Members (MenuTestResources i result) r =>
  Members [Reader (SerialT IO (MenuItem i)), MenuUi, Stop RpcError] r =>
  PromptConfig ->
  (MappingLhs -> Maybe (MenuWidget i (MenuRenderEffects i result ++ r) result)) ->
  InterpretersFor (MenuRenderEffects i result) r
testMenuRender pconf mappings sem = do
  TestTimeout timeout <- ask
  items <- ask
  interpretMenuLoopDeps $ interpretMenuLoops $ interpretMenuTest pconf $ runMenu items $ withEventLog do
    withAsync_ (Sync.putWait timeout =<< menu' mappings) do
      timeout_ (fail "prompt didn't start") timeout waitPrompt
      sem

testMenu ::
  Show i =>
  Members MenuTestIOStack r =>
  Member (Reader (SerialT IO (MenuItem i))) r =>
  Members (MenuTestDeps i result) r =>
  PromptConfig ->
  Mappings i (MenuRenderEffects i result ++ MenuUi : r) result ->
  InterpretersFor (MenuTestEffects i result) r
testMenu pconf maps =
  interpretMenuUiNvimNull .
  resumeReportFail .
  withMenuUi def .
  raiseUnder2 .
  testMenuRender pconf (lookupMapping maps)

testNvimMenu ::
  ∀ i result r .
  Show i =>
  Member (Reader (SerialT IO (MenuItem i))) r =>
  Members MenuTestIOStack r =>
  Members (MenuTestDeps i result) r =>
  Members [Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError] r =>
  PromptConfig ->
  ScratchOptions ->
  Mappings i (MenuRenderEffects i result ++ MenuUi : r) result ->
  InterpretersFor (MenuTestEffects i result) r
testNvimMenu pconf options maps =
  interpretMenuUiNvimEcho .
  resumeReportFail .
  withMenuUi (NvimMenuConfig pconf options (Map.keys maps)) .
  raiseUnder2 .
  testMenuRender pconf (lookupMapping maps)

testStaticNvimMenu ::
  ∀ i result r .
  Show i =>
  Members MenuTestIOStack r =>
  Members [Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  [MenuItem i] ->
  PromptConfig ->
  ScratchOptions ->
  Mappings i (MenuRenderEffects i result ++ MenuUi : MenuTestStack i result ++ r) result ->
  InterpretersFor (MenuTestEffects i result ++ MenuTestStack i result) r
testStaticNvimMenu items pconf options maps =
  runStaticTestMenu pconf items .
  testNvimMenu pconf options maps
