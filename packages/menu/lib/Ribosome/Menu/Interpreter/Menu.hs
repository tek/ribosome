module Ribosome.Menu.Interpreter.Menu where

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
import Ribosome.Host.Effect.MState (MState, ScopedMState, mread, muse)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpret (type (|>))
import Ribosome.Host.Interpreter.MState (interpretMState, interpretMStates)
import qualified Ribosome.Menu.Class.MenuState as MenuState
import Ribosome.Menu.Class.MenuState (Filter, MenuState, MenuState (Item, mode))
import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import qualified Ribosome.Menu.Data.Filter as Filter
import Ribosome.Menu.Data.FilterMode (FilterMode)
import Ribosome.Menu.Data.MenuConfig (MenuConfig, menuSync)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Inserted, PromptLoop, Rendered))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.RenderEvent (RenderEvent (RenderEvent))
import qualified Ribosome.Menu.Data.RenderMenu as RenderMenu
import Ribosome.Menu.Data.RenderMenu (RenderMenu)
import Ribosome.Menu.Data.WithCursor (WithCursor (WithCursor))
import qualified Ribosome.Menu.Effect.Menu as Menu
import Ribosome.Menu.Effect.Menu (Menu, Menus)
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import qualified Ribosome.Menu.Effect.MenuStream as MenuStream
import Ribosome.Menu.Effect.MenuStream (MenuStream)
import Ribosome.Menu.Effect.MenuUi (NvimMenuUi, PureMenu, WindowMenu)
import Ribosome.Menu.Interpreter.MenuFilter (defaultFilter)
import Ribosome.Menu.Interpreter.MenuStream (interpretMenuStream)
import Ribosome.Menu.Interpreter.MenuUiPure (interpretMenuUiPure)
import Ribosome.Menu.Interpreter.MenuUiWindow (interpretMenuUiWindow)
import qualified Ribosome.Menu.Prompt.Data.Prompt as Prompt
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.UpdateState (insertItems, queryEvent)

newtype MS s =
  MS { unMS :: s }
  deriving stock (Eq, Show)

viaMS ::
  Functor m =>
  (s -> m (s, a)) ->
  MS s ->
  m (MS s, a)
viaMS f (MS s) =
  first MS <$> f s

msState ::
  Member (MState (MS s)) r =>
  InterpreterFor (State s) r
msState sem =
  muse (viaMS (flip runState sem))

mstateT ::
  Functor f =>
  (s -> m (s, a)) ->
  s ->
  Sem (WithTactics e f m r) (s, f a)
mstateT f s = do
  res <- runTSimple (f s)
  Inspector ins <- getInspectorT
  let newS = fromMaybe s (fst <$> ins res)
  pure (newS, snd <$> res)

data MenuSync =
  MenuSync
  deriving stock (Eq, Show)

renderEvent ::
  MenuState s =>
  Members [MState (MS s), MState CursorIndex, Events eres MenuEvent, Log] r =>
  (RenderMenu (Item s) -> Sem r ()) ->
  RenderEvent ->
  Sem r ()
renderEvent render (RenderEvent desc) = do
  Log.debug [exon|menu render: #{desc}|]
  MS s <- mread
  c <- mread
  render (RenderMenu.fromState (WithCursor s c))
  publish Rendered

-- |Call the effect that hides the streaming internals by passing all actions to it so they don't need to be lowered
-- with 'withStrategicToFinal'.
--
-- - @Queue.readMaybe@ pulls 'Prompt' events emitted by 'Menu'.
--
-- - @queryUpdate@ applies a new query to the current set ostems.
--
-- - @insert@ adds new items to the set.
--
-- - @Queue.write@ sends 'RenderEvent's to 'renderEvent'.
--
-- - @publish@ sends 'MenuEvent's to consumers of 'Events'.
menuStream ::
  MenuState s =>
  Members [Queue (Maybe Prompt), Queue RenderEvent, Events ires MenuEvent] r =>
  Members [MenuStream, MenuFilter (Filter s), MState (MS s), Sync MenuSync, Log] r =>
  SerialT IO (MenuItem (Item s)) ->
  Sem r ()
menuStream items = do
  Log.debug "Starting menu stream"
  MenuStream.menuStream items Queue.readMaybe update insert Queue.write (publish MenuEvent.Exhausted)
  Log.debug "Finished menu stream"
  where
    insert new = do
      msState (insertItems new)
      RenderEvent "new items" <$ publish Inserted
    update p = do
      msState (queryEvent (Prompt.text <$> p))
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
  NvimMenuUi WindowMenu : MenuFilter (FilterMode Filter.Filter) : MenuLoopDeps

type NvimMenu s =
  Menus s : NvimMenuUi WindowMenu : MenuLoopDeps

interpretMenus ::
  Members MenuLoopIO r =>
  Members [Settings !! SettingError, Rpc !! RpcError, Scratch !! RpcError, EventConsumer eres Event, Final IO] r =>
  InterpretersFor NvimMenus r
interpretMenus =
  interpretMenuLoopDeps .
  defaultFilter .
  interpretMenuUiWindow

type MenuLoopScope s =
  [Gate @@ "prompt", MState (MS s), MState CursorIndex, Sync MenuSync, Queue (Maybe Prompt), Queue RenderEvent]

menuLoopScope ::
  MenuState s =>
  Members MenuLoopIO r =>
  Members MenuLoopDeps r =>
  Member (MenuFilter (Filter s)) r =>
  SerialT IO (MenuItem (Item s)) ->
  s ->
  (() -> Sem (MenuLoopScope s ++ r) a) ->
  Sem r a
menuLoopScope items initial use =
  interpretQueueTBM 64 $
  interpretQueueTBM 64 $
  interpretSync $
  interpretMState def .
  interpretMState (MS initial) .
  withAsync_ (menuStream items) $
  withGate $ untag $
  use ()

interpretMenuLoops ::
  ∀ s r .
  MenuState s =>
  Members MenuLoopIO r =>
  Members MenuLoopDeps r =>
  Member (MenuFilter (Filter s)) r =>
  InterpreterFor (Menus s) r
interpretMenuLoops =
  interpretPScopedWithH @(MenuLoopScope s) (uncurry menuLoopScope) \ () ->
    let
      handle :: ∀ r0 x . Menu s (Sem r0) x -> Tactical (Menu s) (Sem r0) (MenuLoopScope s ++ r) x
      handle = \case
        Menu.WithRender render ma -> do
          s <- getInitialStateT
          render' <- bindT render <&> \ f -> interpretH handle . f
          ma' <- interpretH handle <$> runT ma
          raise (withAsync_ (Queue.loop (renderEvent \ m -> void (render' (m <$ s)))) ma')
        Menu.ReadCursor ->
          pureT =<< mread
        Menu.UseCursor f ->
          muse (mstateT f)
        Menu.ReadState ->
          pureT . unMS =<< mread
        Menu.UseState f ->
          muse $ viaMS \ s -> do
            (newS, a) <- mstateT f s
            when (s ^. mode /= newS ^. mode) (Queue.write Nothing)
            pure (newS, a)
        Menu.StartPrompt ->
          pureT =<< tag @"prompt" signal
        Menu.WaitPrompt ->
          pureT =<< tag @"prompt" gate
        Menu.PromptQuit ->
          pureT =<< Queue.close
        Menu.PromptUpdated new -> do
          publish (MenuEvent.PromptUpdated new)
          Queue.write (Just new)
          whenM (asks (view #sync)) (void Sync.takeBlock)
          unitT
        Menu.Render ->
          Queue.write (RenderEvent "consumer") *> unitT
        Menu.PromptLooped ->
          pureT =<< publish PromptLoop
    in handle

type NvimMenuIO eres =
  [Settings !! SettingError, Rpc !! RpcError, Scratch !! RpcError, EventConsumer eres Event, Final IO]

interpretNvimMenu ::
  MenuState s =>
  Members MenuLoopIO r =>
  Member (MenuFilter (Filter s)) r =>
  Members (NvimMenuIO eres) r =>
  InterpretersFor (NvimMenu s) r
interpretNvimMenu =
  interpretMenuLoopDeps .
  interpretMenuUiWindow .
  interpretMenuLoops

promptInputFilter ::
  MenuState s =>
  Members MenuLoopIO r =>
  Members (NvimMenuIO eres) r =>
  Member (MenuFilter (Filter s)) r =>
  [PromptEvent] ->
  InterpretersFor (NvimMenuUi PureMenu : Menus s : MenuLoopDeps |> ChronosTime) r
promptInputFilter events =
  interpretTimeChronos .
  interpretMenuLoopDeps .
  interpretMenuLoops .
  interpretMenuUiPure True events .
  menuSync

promptInput ::
  MenuState s =>
  MenuState.Filter s ~ FilterMode Filter.Filter =>
  Members MenuLoopIO r =>
  Members (NvimMenuIO eres) r =>
  [PromptEvent] ->
  InterpretersFor (NvimMenuUi PureMenu : Menus s : MenuLoopDeps ++ [MenuFilter (MenuState.Filter s), ChronosTime]) r
promptInput events =
  interpretTimeChronos .
  defaultFilter .
  interpretMenuLoopDeps .
  interpretMenuLoops .
  interpretMenuUiPure True events .
  menuSync
