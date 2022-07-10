module Ribosome.Menu.Interpreter.MenuState where

import Conc (Lock, PScoped, interpretAtomic, interpretScopedWithH, interpretSync, lock)

import Ribosome.Data.ScratchId (ScratchId)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.MState (MState, ScopedMState, mread, muse)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpreter.MState (interpretMState, interpretMStates)
import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.MenuConfig (MenuConfig)
import Ribosome.Menu.Data.MenuData (MenuItems)
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer)
import Ribosome.Menu.Effect.MenuState (MenuState (..), ScopedMenuState, withMenuState)
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
import Ribosome.Menu.Prompt.Data.PromptListening (PromptListening)
import Ribosome.Menu.Prompt.Data.PromptQuit (PromptQuit)
import Ribosome.Menu.Prompt.Nvim (NvimPromptResources)

type MenuStateDeps i =
  [
    Sync PromptQuit,
    Sync PromptListening,
    MState (MenuItems i),
    MState CursorIndex,
    AtomicState Prompt
  ]

interpretMenuStateDeps ::
  ∀ i mres r .
  Members [Resource, Race, Mask mres, Embed IO] r =>
  InterpretersFor (MenuStateDeps i) r
interpretMenuStateDeps =
  interpretAtomic def .
  interpretMState def .
  interpretMState def .
  interpretSync @PromptListening .
  interpretSync @PromptQuit

itemsLock ::
  Members [Tagged "items" Lock, Resource] r =>
  Sem r a ->
  Sem r a
itemsLock =
  tag . lock . raise

cursorLock ::
  Members [Tagged "cursor" Lock, Resource] r =>
  Sem r a ->
  Sem r a
cursorLock =
  tag . lock . raise

mcState ::
  Functor f =>
  Member (MState s) r =>
  (s -> m (s, a)) ->
  Sem (WithTactics e f m r) (f a)
mcState f =
  muse \ s -> do
    res <- runTSimple (f s)
    Inspector ins <- getInspectorT
    let newS = fromMaybe s (fst <$> ins res)
    pure (newS, snd <$> res)

handleMenuState ::
  Members (MenuStateDeps i) r =>
  MenuState i m x ->
  Tactical (MenuState i) m r x
handleMenuState = \case
  UseCursor f ->
    mcState f
  ReadCursor ->
    pureT =<< mread
  SetPrompt p ->
    pureT =<< atomicPut p
  ReadPrompt ->
    pureT =<< atomicGet
  UseItems f ->
    mcState f
  ReadItems ->
    pureT =<< mread

interpretMenuState ::
  ∀ i mres r .
  Members [Resource, Race, Mask mres, Embed IO] r =>
  InterpreterFor (MenuState i) r
interpretMenuState =
  interpretMenuStateDeps .
  interpretH handleMenuState .
  insertAt @1

scope ::
  Members [Resource, Race, Mask mres, Embed IO] r =>
  (() -> Sem (MenuStateDeps i ++ r) a) ->
  Sem r a
scope use =
  interpretMenuStateDeps do
    use ()

interpretMenuStates ::
  ∀ i mres r .
  Members [Resource, Race, Mask mres, Embed IO] r =>
  InterpreterFor (ScopedMenuState i) r
interpretMenuStates =
  interpretScopedWithH @(MenuStateDeps i) scope \ () e -> handleMenuState e

type MenuStack i =
  [MenuState i, PromptControl, PromptState, PromptEvents, Reader (MenuConfig i)]

type MenuIOEffects i =
  [
    MenuStream,
    PromptStream,
    MenuFilter,
    Scoped () PromptControl,
    Scoped () (MenuState i),
    ScopedMState Prompt
  ]

type MenuIOStack i =
  MenuStack i ++ MenuIOEffects i

type NvimMenuIOEffects i =
  [
    PScoped ScratchOptions ScratchId (MenuRenderer i),
    Scoped NvimPromptResources PromptRenderer,
    NvimPromptInput
  ]

type NvimMenuIOStack i =
  PromptInput : MenuIOStack i ++ NvimMenuIOEffects i

runMenu ::
  ∀ i r .
  Members [Scoped () PromptControl, ScopedMState Prompt, Scoped () (MenuState i), Log] r =>
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
  Members [NvimPromptInput, Scoped () PromptControl, ScopedMState Prompt, Scoped () (MenuState i), Log] r =>
  MenuConfig i ->
  [PromptFlag] ->
  InterpretersFor (PromptInput : MenuStack i) r
runNvimMenu conf flags =
  runMenu conf flags .
  nvimPromptInput

interpretMenuFinal ::
  ∀ i mres r .
  Members [Log, Resource, Race, Mask mres, Embed IO, Final IO] r =>
  InterpretersFor (MenuIOEffects i) r
interpretMenuFinal =
  interpretMStates .
  interpretMenuStates .
  interpretPromptControl .
  interpretMenuFilterFuzzy @'True .
  interpretPromptStream .
  interpretMenuStream

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
  Members [Settings !! SettingError, Scratch] r =>
  Members [Rpc, Rpc !! RpcError, Log, Resource, Race, Mask mres, Embed IO, Final IO] r =>
  InterpretersFor (NvimMenuIOEffects i) r
interpretNvimMenuIOEffects =
  interpretNvimPromptInput .
  interpretPromptRendererNvim .
  interpretMenuRendererNvim

interpretNvimMenuFinal ::
  ∀ i mres r .
  Members [Settings !! SettingError, Scratch] r =>
  Members [Rpc, Rpc !! RpcError, Log, Resource, Race, Mask mres, Embed IO, Final IO] r =>
  InterpretersFor (MenuIOEffects i ++ NvimMenuIOEffects i) r
interpretNvimMenuFinal =
  interpretNvimMenuIOEffects .
  interpretMenuFinal

runNvimMenuFinal ::
  ∀ i mres r .
  Members [Settings !! SettingError, Scratch] r =>
  Members [Rpc, Rpc !! RpcError, Log, Resource, Race, Mask mres, Embed IO, Final IO] r =>
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
