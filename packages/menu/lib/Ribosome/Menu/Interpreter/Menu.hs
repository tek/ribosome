module Ribosome.Menu.Interpreter.Menu where

import Conc (PScoped)

import Ribosome.Data.ScratchId (ScratchId)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.MState (ScopedMState)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpreter.MState (interpretMStates)
import Ribosome.Menu.Data.MenuConfig (MenuConfig)
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer)
import Ribosome.Menu.Effect.MenuState (MenuState (..), withMenuState)
import Ribosome.Menu.Effect.MenuStream (MenuStream)
import Ribosome.Menu.Effect.NvimPromptInput (NvimPromptInput)
import Ribosome.Menu.Effect.PromptControl (PromptControl, withPromptControl)
import Ribosome.Menu.Effect.PromptEvents (PromptEvents)
import Ribosome.Menu.Effect.PromptInput (PromptInput)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer)
import Ribosome.Menu.Effect.PromptState (PromptState)
import Ribosome.Menu.Effect.PromptStream (PromptStream)
import Ribosome.Menu.Interpreter.MenuFilter (interpretMenuFilterFuzzy)
import Ribosome.Menu.Interpreter.MenuRenderer (interpretMenuRendererNvim)
import Ribosome.Menu.Interpreter.MenuState (interpretMenuStates)
import Ribosome.Menu.Interpreter.MenuStream (interpretMenuStream)
import Ribosome.Menu.Interpreter.NvimPromptInput (interpretNvimPromptInput)
import Ribosome.Menu.Interpreter.PromptControl (interpretPromptControl)
import Ribosome.Menu.Interpreter.PromptEvents (interpretPromptEventsDefault)
import Ribosome.Menu.Interpreter.PromptInput (nvimPromptInput)
import Ribosome.Menu.Interpreter.PromptRenderer (interpretPromptRendererNvim)
import Ribosome.Menu.Interpreter.PromptState (interpretPromptState)
import Ribosome.Menu.Interpreter.PromptStream (interpretPromptStream)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptFlag (PromptFlag)
import Ribosome.Menu.Prompt.Nvim (NvimPromptResources)

type MenuStack i =
  [MenuState i, PromptControl, PromptState, PromptEvents, Reader (MenuConfig i)]

type NvimMenuStack i =
  PromptInput : MenuStack i

type MenuDeps i =
  [
    Scoped () PromptControl,
    ScopedMState Prompt,
    Scoped () (MenuState i),
    Log
  ]

type NvimMenuDeps i =
  NvimPromptInput : MenuDeps i

type MenusIOEffects =
  [
    MenuStream,
    PromptStream,
    MenuFilter,
    Scoped () PromptControl,
    ScopedMState Prompt
  ]

type MenuIOEffects i =
  Scoped () (MenuState i) : MenusIOEffects

type MenuIOStack i =
  MenuStack i ++ MenuIOEffects i

type NvimMenusIOEffects =
  [
    Scoped NvimPromptResources PromptRenderer !! RpcError,
    NvimPromptInput
  ]

type NvimMenuIOEffects i =
  PScoped ScratchOptions ScratchId (MenuRenderer i) !! RpcError : NvimMenusIOEffects

type NvimMenuIOStack i =
  PromptInput : MenuIOStack i ++ NvimMenuIOEffects i

runMenu ::
  ∀ i r .
  Members (MenuDeps i) r =>
  MenuConfig i ->
  [PromptFlag] ->
  InterpretersFor (MenuStack i) r
runMenu conf flags =
  runReader conf .
  interpretPromptEventsDefault flags .
  interpretPromptState flags .
  withPromptControl .
  withMenuState

runNvimMenu ::
  ∀ i r .
  Members (NvimMenuDeps i) r =>
  MenuConfig i ->
  [PromptFlag] ->
  InterpretersFor (NvimMenuStack i) r
runNvimMenu conf flags =
  runMenu conf flags .
  nvimPromptInput

interpretMenusFinal ::
  ∀ mres r .
  Members [Log, Resource, Race, Mask mres, Embed IO, Final IO] r =>
  InterpretersFor (MenusIOEffects) r
interpretMenusFinal =
  interpretMStates .
  interpretPromptControl .
  interpretMenuFilterFuzzy @'True .
  interpretPromptStream .
  interpretMenuStream

interpretMenuFinal ::
  ∀ i mres r .
  Members [Log, Resource, Race, Mask mres, Embed IO, Final IO] r =>
  InterpretersFor (MenuIOEffects i) r
interpretMenuFinal =
  interpretMenusFinal .
  interpretMenuStates

runMenuFinal ::
  ∀ i mres r .
  Members [Log, Resource, Race, Mask mres, Embed IO, Final IO] r =>
  MenuConfig i ->
  [PromptFlag] ->
  InterpretersFor (MenuIOStack i) r
runMenuFinal conf flags =
  interpretMenuFinal .
  runMenu conf flags

interpretNvimMenuIOEffects ::
  ∀ i mres r .
  Members [Settings !! SettingError, Scratch !! RpcError] r =>
  Members [Rpc !! RpcError, Log, Resource, Race, Mask mres, Embed IO, Final IO] r =>
  InterpretersFor (NvimMenuIOEffects i) r
interpretNvimMenuIOEffects =
  interpretNvimPromptInput .
  interpretPromptRendererNvim .
  interpretMenuRendererNvim

interpretNvimMenusFinal ::
  ∀ mres r .
  Members [Rpc !! RpcError, Log, Resource, Race, Mask mres, Embed IO, Final IO] r =>
  InterpretersFor (MenusIOEffects ++ NvimMenusIOEffects) r
interpretNvimMenusFinal =
  interpretNvimPromptInput .
  interpretPromptRendererNvim .
  interpretMenusFinal

interpretNvimMenuFinal ::
  ∀ i mres r .
  Members [Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError] r =>
  Members [Log, Resource, Race, Mask mres, Embed IO, Final IO] r =>
  InterpretersFor (MenuIOEffects i ++ NvimMenuIOEffects i) r
interpretNvimMenuFinal =
  interpretNvimMenuIOEffects .
  interpretMenuFinal

runNvimMenuFinal ::
  ∀ i mres r .
  Members [Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError] r =>
  Members [Log, Resource, Race, Mask mres, Embed IO, Final IO] r =>
  MenuConfig i ->
  [PromptFlag] ->
  InterpretersFor (NvimMenuIOStack i) r
runNvimMenuFinal conf flags =
  interpretNvimPromptInput .
  interpretPromptRendererNvim .
  interpretMenuRendererNvim .
  interpretMenuFinal .
  runMenu conf flags .
  nvimPromptInput
