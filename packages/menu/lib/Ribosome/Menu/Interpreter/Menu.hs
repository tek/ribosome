module Ribosome.Menu.Interpreter.Menu where

import Conc (PScoped)
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (SerialT)

import Ribosome.Data.ScratchId (ScratchId)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.MState (ScopedMState)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpreter.MState (interpretMStates)
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer, withMenuRenderer)
import Ribosome.Menu.Effect.MenuState (MenuState (..), withMenuState)
import Ribosome.Menu.Effect.MenuStream (MenuStream)
import Ribosome.Menu.Effect.NvimPromptInput (NvimPromptInput)
import Ribosome.Menu.Effect.PromptControl (PromptControl, withPromptControl)
import Ribosome.Menu.Effect.PromptEvents (PromptEvents)
import Ribosome.Menu.Effect.PromptInput (PromptInput)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer, withPrompt)
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
import Ribosome.Menu.Nvim (ensureSize, interpretNvimMenu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptFlag (PromptFlag)
import Ribosome.Menu.Prompt.Nvim (NvimPromptResources)

type MenuStack i =
  [MenuState i, PromptControl, PromptState, PromptEvents, Reader (MenuConfig i)]

type NvimRenderers i =
  [
    PromptRenderer,
    MenuRenderer i
  ]

type NvimMenuStack i =
  NvimRenderers i ++ PromptInput : MenuStack i

type MenuDeps i =
  [
    Scoped () PromptControl,
    ScopedMState Prompt,
    Scoped () (MenuState i),
    Log
  ]

type ScopedNvimRenderers i =
  [
    Scoped NvimPromptResources PromptRenderer !! RpcError,
    PScoped ScratchOptions ScratchId (MenuRenderer i) !! RpcError
  ]

type NvimMenuDeps i =
  ScopedNvimRenderers i ++ NvimPromptInput : MenuDeps i

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
  PromptRenderer : MenuRenderer i : PromptInput : MenuIOStack i ++ NvimMenuIOEffects i

runMenu ::
  ∀ i r .
  Members (MenuDeps i) r =>
  SerialT IO (MenuItem i) ->
  [PromptFlag] ->
  InterpretersFor (MenuStack i) r
runMenu conf flags =
  runReader (MenuConfig conf) .
  interpretPromptEventsDefault flags .
  interpretPromptState flags .
  withPromptControl .
  withMenuState

interpretNvimRenderers ::
  Member (Stop RpcError) r =>
  Members (ScopedNvimRenderers i) r =>
  ScratchOptions ->
  InterpretersFor (NvimRenderers i) r
interpretNvimRenderers options =
  restop .
  withMenuRenderer (ensureSize 1 options) .
  raiseUnder .
  restop .
  withPrompt .
  raiseUnder

runNvimMenu ::
  ∀ i r .
  Member (Stop RpcError) r =>
  Members (NvimMenuDeps i) r =>
  SerialT IO (MenuItem i) ->
  [PromptFlag] ->
  ScratchOptions ->
  InterpretersFor (NvimMenuStack i) r
runNvimMenu items flags options =
  runMenu items flags .
  nvimPromptInput .
  interpretNvimRenderers options

runStaticNvimMenu ::
  ∀ i r .
  Member (Stop RpcError) r =>
  Members (NvimMenuDeps i) r =>
  [MenuItem i] ->
  [PromptFlag] ->
  ScratchOptions ->
  InterpretersFor (NvimMenuStack i) r
runStaticNvimMenu items flags options =
  runNvimMenu (Stream.fromList items) flags (ensureSize (length items) options)

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
  SerialT IO (MenuItem i) ->
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
  interpretNvimMenu

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
  Members [Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Stop RpcError] r =>
  Members [Log, Resource, Race, Mask mres, Embed IO, Final IO] r =>
  SerialT IO (MenuItem i) ->
  [PromptFlag] ->
  ScratchOptions ->
  InterpretersFor (NvimMenuIOStack i) r
runNvimMenuFinal conf flags options =
  interpretNvimPromptInput .
  interpretPromptRendererNvim .
  interpretMenuRendererNvim .
  interpretMenuFinal .
  runNvimMenu conf flags options
