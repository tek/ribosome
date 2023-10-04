module Ribosome.Menu.Test.Main where

import Conc (Gates)

import Ribosome.Data.Mapping (MappingSpec)
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
import qualified Ribosome.Menu.App
import Ribosome.Menu.App (MenuApp, hoistPromptApp)
import Ribosome.Menu.Class.MenuState (MenuState (Item))
import Ribosome.Menu.Data.TestMenuConfig (TestMenuConfig (..), confDefault)
import Ribosome.Menu.Data.WindowConfig (WindowConfig (WindowConfig))
import Ribosome.Menu.Effect.Menu (MenuLoop, UiMenus)
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import Ribosome.Menu.Effect.MenuTest (MenuTest, MenuTests)
import Ribosome.Menu.Effect.MenuUi (ScopedMenuUi)
import Ribosome.Menu.Interpreter.Menu (MenuLoopDeps, MenuLoopIO, interpretMenus)
import Ribosome.Menu.Interpreter.MenuFilter (interpretFilter)
import Ribosome.Menu.Interpreter.MenuUi (interpretMenuUiNvimNull)
import Ribosome.Menu.Interpreter.MenuUiWindow (interpretMenuUiWindow)
import Ribosome.Menu.Loop (menuAppWith, withUi)
import Ribosome.Menu.Test.Loop (promptTestApp)

type MenuTestIO =
  MenuLoopIO ++ [Fail, Stop RpcError, Gates, Final IO]

type TestLoop s result =
  MenuLoop s |> MenuTest (Item s) result

type MenuTestLocal ui s result =
  [
    UiMenus ui s,
    UiMenus ui s !! RpcError,
    MenuFilter,
    ReportLog
  ]

type MenuTestMain s result =
  MenuTests (Item s) result : MenuLoopDeps ++ MenuTestIO

type NvimMenuTest =
  [EventConsumer Event, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError]

interpretMenuTestLocal ::
  ∀ ui s result r .
  MenuState s =>
  Members MenuLoopIO r =>
  Members MenuLoopDeps r =>
  Members [ScopedMenuUi ui, Fail] r =>
  InterpretersFor (MenuTestLocal ui s result) r
interpretMenuTestLocal =
  interpretReportLogLog .
  interpretFilter .
  interpretMenus .
  resumeReportFail

testMenu ::
  ∀ ui result s r .
  MenuState s =>
  Members (MenuTestMain s result) r =>
  Member (ScopedMenuUi ui) r =>
  ([MappingSpec] -> ui) ->
  TestMenuConfig (Item s) ->
  s ->
  MenuApp s r result ->
  InterpretersFor (TestLoop s result) r
testMenu ui conf initial app sem =
  interpretMenuTestLocal $ scoped @_ @(MenuTest (Item s) result) conf do
    menuAppWith addBuiltin addDefault prompt app \ papp ->
      withUi (ui papp.mappings) do
        promptTestApp conf initial (hoistPromptApp (insertAt @2) papp) $ insertAt @5 $ raise3Under sem
  where
    addBuiltin = fromMaybe True conf.addBuiltin
    addDefault = fromMaybe True conf.addDefault
    prompt = fromMaybe def conf.prompt

headlessTestMenu ::
  ∀ result s r .
  MenuState s =>
  Members (MenuTestMain s result) r =>
  TestMenuConfig (Item s) ->
  s ->
  MenuApp s r result ->
  InterpretersFor (TestLoop s result) r
headlessTestMenu conf initial app =
  interpretMenuUiNvimNull .
  testMenu (const ()) conf' initial (raise2Under <$> app) .
  insertAt @4
  where
    conf' = confDefault #nativePrompt False conf

nvimTestMenu ::
  ∀ result s r .
  MenuState s =>
  Members (MenuTestMain s result) r =>
  Members NvimMenuTest r =>
  TestMenuConfig (Item s) ->
  s ->
  ScratchOptions ->
  MenuApp s r result ->
  InterpretersFor (TestLoop s result) r
nvimTestMenu conf initial options app =
  interpretMenuUiWindow .
  testMenu (WindowConfig options (Just def)) conf' initial (insertAt @2 <$> app) .
  insertAt @4
  where
    conf' = confDefault #prompt def conf
