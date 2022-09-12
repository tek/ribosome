module Ribosome.Menu.Interpreter.MenuLoop where

import Conc (
  ChanConsumer,
  ChanEvents,
  Gate,
  GatesIO,
  MaskIO,
  interpretEventsChan,
  interpretGates,
  interpretPScopedWithH,
  interpretQueueTBM,
  interpretSync,
  subscribeLoopAsync,
  withAsync_,
  )
import Exon (exon)
import Lens.Micro.Mtl (view)
import qualified Log
import Polysemy.Chronos (ChronosTime, interpretTimeChronos)
import Polysemy.Conc.Gate (gate, signal, withGate)
import qualified Queue
import Streamly.Prelude (SerialT)
import qualified Sync

import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.Event (Event)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.MState (ScopedMState)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpret (type (|>))
import Ribosome.Host.Interpreter.MState (interpretMStates)
import Ribosome.Menu.Class.FilterEnum (FilterEnum)
import Ribosome.Menu.Data.Filter (Filter)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuConfig (MenuConfig, menuSync)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Inserted, PromptLoop, Rendered))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.RenderEvent (RenderEvent (RenderEvent))
import Ribosome.Menu.Data.RenderMenu (fromMenu)
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import qualified Ribosome.Menu.Effect.MenuLoop as MenuLoop
import Ribosome.Menu.Effect.MenuLoop (MenuLoop, MenuLoops)
import qualified Ribosome.Menu.Effect.MenuState as MenuState
import Ribosome.Menu.Effect.MenuState (MenuState, itemsState, readMenu)
import qualified Ribosome.Menu.Effect.MenuStream as MenuStream
import Ribosome.Menu.Effect.MenuStream (MenuStream)
import Ribosome.Menu.Effect.MenuUi (NvimMenuUi, PureMenu, WindowMenu)
import Ribosome.Menu.Interpreter.MenuFilter (defaultFilter)
import Ribosome.Menu.Interpreter.MenuState (interpretMenuState, mstateT)
import Ribosome.Menu.Interpreter.MenuStream (interpretMenuStream)
import Ribosome.Menu.Interpreter.MenuUiPure (interpretMenuUiPure)
import Ribosome.Menu.Interpreter.MenuUiWindow (interpretMenuUiWindow)
import qualified Ribosome.Menu.Prompt.Data.Prompt as Prompt
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.UpdateState (changeFilter, insertItems, queryUpdate)

data MenuSync =
  MenuSync
  deriving stock (Eq, Show)

renderEvent ::
  Members [MenuState f i, Events eres MenuEvent, Log] r =>
  (Menu f i -> Sem r ()) ->
  RenderEvent ->
  Sem r ()
renderEvent render (RenderEvent desc) = do
  Log.debug [exon|menu render: #{desc}|]
  render =<< readMenu
  publish Rendered

-- |Call the effect that hides the streaming internals by passing all actions to it so they don't need to be lowered
-- with 'withStrategicToFinal'.
--
-- - @Queue.readMaybe@ pulls 'Prompt' events emitted by 'MenuLoop'.
--
-- - @queryUpdate@ applies a new query to the current set of items.
--
-- - @insert@ adds new items to the set.
--
-- - @Queue.write@ sends 'RenderEvent's to 'renderEvent'.
--
-- - @publish@ sends 'MenuEvent's to consumers of 'Events'.
menuStream ::
  Ord f =>
  Members [Queue Prompt, Queue RenderEvent, Events ires MenuEvent] r =>
  Members [MenuStream, MenuFilter f, MenuState f i, Sync MenuSync, Log] r =>
  SerialT IO (MenuItem i) ->
  Sem r ()
menuStream items = do
  Log.debug "Starting menu stream"
  MenuStream.menuStream items Queue.readMaybe update insert Queue.write publish
  Log.debug "Finished menu stream"
  where
    insert new = do
      itemsState (insertItems new)
      publish Inserted
    update p = do
      itemsState (queryUpdate (Prompt.text p))
      RenderEvent "query update" <$ Sync.putTry MenuSync

type MenuLoopIO =
  [
    Log,
    MaskIO,
    Resource,
    Race,
    Async,
    Embed IO
  ]

type MenuLoopDeps =
  [
    MenuStream,
    Reader MenuConfig,
    ScopedMState Prompt,
    ChanEvents MenuEvent,
    ChanConsumer MenuEvent,
    GatesIO
  ]

interpretMenuLoopDeps ::
  Member (Final IO) r =>
  Members MenuLoopIO r =>
  InterpretersFor MenuLoopDeps r
interpretMenuLoopDeps =
  interpretGates .
  interpretEventsChan .
  interpretMStates .
  runReader def .
  interpretMenuStream

type NvimMenus =
  NvimMenuUi WindowMenu : MenuLoopDeps

type NvimMenu f i =
  MenuLoops f i : NvimMenus

interpretMenus ::
  Members MenuLoopIO r =>
  Members [Settings !! SettingError, Rpc !! RpcError, Scratch !! RpcError, EventConsumer eres Event, Final IO] r =>
  InterpretersFor NvimMenus r
interpretMenus =
  interpretMenuLoopDeps .
  interpretMenuUiWindow

type MenuLoopScope f i =
  [Gate @@ "prompt", MenuState f i, Sync MenuSync, Queue Prompt, Queue RenderEvent]

menuLoopScope ::
  Ord f =>
  Members MenuLoopIO r =>
  Members MenuLoopDeps r =>
  Member (MenuFilter f) r =>
  SerialT IO (MenuItem i) ->
  f ->
  (() -> Sem (MenuLoopScope f i ++ r) a) ->
  Sem r a
menuLoopScope items initialFilter use =
  interpretQueueTBM 64 $
  interpretQueueTBM 64 $
  interpretSync $
  interpretMenuState initialFilter $
  subscribeLoopAsync (\ event -> Log.debug [exon|menu event: #{show event}|]) $
  withAsync_ (menuStream items) $
  withGate $ untag $
  use ()

interpretMenuLoops ::
  ∀ i f r .
  FilterEnum f =>
  Members MenuLoopIO r =>
  Members MenuLoopDeps r =>
  Member (MenuFilter f) r =>
  InterpreterFor (MenuLoops f i) r
interpretMenuLoops =
  interpretPScopedWithH @(MenuLoopScope f i) (uncurry menuLoopScope) \ () ->
    let
      int :: ∀ r0 x . MenuLoop f i (Sem r0) x -> Tactical (MenuLoop f i) (Sem r0) (MenuLoopScope f i ++ r) x
      int = \case
        MenuLoop.WithRender render ma -> do
          s <- getInitialStateT
          render' <- bindT render <&> \ f -> interpretH int . f
          ma' <- interpretH int <$> runT ma
          raise (withAsync_ (Queue.loop (renderEvent \ m -> void (render' (fromMenu m <$ s)))) ma')
        MenuLoop.ReadCursor ->
          pureT =<< MenuState.readCursor
        MenuLoop.UseCursor f ->
          MenuState.useCursor (mstateT f)
        MenuLoop.ReadItems ->
          pureT =<< MenuState.readItems
        MenuLoop.UseItems f ->
          MenuState.useItems (mstateT f)
        MenuLoop.ChangeFilter f -> do
          itemsState (changeFilter f)
          Queue.write (RenderEvent "change filter")
          unitT
        MenuLoop.StartPrompt ->
          pureT =<< tag @"prompt" signal
        MenuLoop.WaitPrompt ->
          pureT =<< tag @"prompt" gate
        MenuLoop.PromptQuit ->
          pureT =<< Queue.close
        MenuLoop.PromptUpdated new -> do
          publish (MenuEvent.PromptUpdated new)
          Queue.write new
          whenM (asks (view #sync)) (void Sync.takeBlock)
          unitT
        MenuLoop.Render ->
          Queue.write (RenderEvent "consumer") *> unitT
        MenuLoop.PromptLooped ->
          pureT =<< publish PromptLoop
    in int

type NvimMenuIO eres =
  [Settings !! SettingError, Rpc !! RpcError, Scratch !! RpcError, EventConsumer eres Event, Final IO]

interpretNvimMenu ::
  FilterEnum f =>
  Members MenuLoopIO r =>
  Member (MenuFilter f) r =>
  Members (NvimMenuIO eres) r =>
  InterpretersFor (NvimMenu f i) r
interpretNvimMenu =
  interpretMenus .
  interpretMenuLoops

promptInputFilter ::
  FilterEnum f =>
  Members MenuLoopIO r =>
  Members (NvimMenuIO eres) r =>
  Member (MenuFilter f) r =>
  [PromptEvent] ->
  InterpretersFor (NvimMenuUi PureMenu : MenuLoops f i : MenuLoopDeps |> ChronosTime) r
promptInputFilter events =
  interpretTimeChronos .
  interpretMenuLoopDeps .
  interpretMenuLoops .
  interpretMenuUiPure True events .
  menuSync

promptInput ::
  Members MenuLoopIO r =>
  Members (NvimMenuIO eres) r =>
  [PromptEvent] ->
  InterpretersFor (NvimMenuUi PureMenu : MenuLoops Filter i : MenuLoopDeps ++ [MenuFilter Filter, ChronosTime]) r
promptInput events =
  interpretTimeChronos .
  defaultFilter .
  interpretMenuLoopDeps .
  interpretMenuLoops .
  interpretMenuUiPure True events .
  menuSync
