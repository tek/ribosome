module Ribosome.Menu.Interpreter.Menu where

import Conc (Gate, Gates, interpretEventsChan, interpretGates, interpretQueueTBM, interpretSync, withAsync_)
import Exon (exon)
import Lens.Micro.Mtl (view)
import qualified Log
import Polysemy.Bundle (Bundle (Bundle))
import Polysemy.Chronos (ChronosTime, interpretTimeChronos)
import Polysemy.Conc.Gate (gate, signal, withGate)
import Polysemy.Membership (ElemOf (Here, There))
import qualified Queue
import Streamly.Prelude (SerialT)
import qualified Sync

import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.Event (Event)
import Ribosome.Host.Data.RpcError (RpcError, rpcError)
import Ribosome.Host.Effect.MState (MState, ScopedMState, mread, mstate, muse)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpret (type (|>))
import Ribosome.Host.Interpreter.MState (interpretMState, interpretMStates)
import Ribosome.Menu.Class.MenuState (MenuState (Item, mode), entries)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import Ribosome.Menu.Data.Entry (entriesLength)
import Ribosome.Menu.Data.MenuAction (RenderAnchor (AnchorLine))
import Ribosome.Menu.Data.MenuConfig (MenuConfig, menuSync)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Inserted, PromptLoop, Rendered))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.RenderEvent (RenderEvent (RenderEvent))
import qualified Ribosome.Menu.Data.RenderMenu as RenderMenu
import Ribosome.Menu.Data.WindowConfig (WindowConfig)
import Ribosome.Menu.Data.WithCursor (WithCursor (WithCursor))
import qualified Ribosome.Menu.Effect.Menu as Menu
import Ribosome.Menu.Effect.Menu (
  MenuEngine (MenuEngine),
  MenuParams (MenuParams),
  UiMenuParams (UiMenuParams),
  UiMenus,
  WindowMenus,
  )
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import qualified Ribosome.Menu.Effect.MenuStream as MenuStream
import Ribosome.Menu.Effect.MenuStream (MenuStream)
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Effect.MenuUi (MenuUi, ScopedMenuUi)
import Ribosome.Menu.Integral (subClamp)
import Ribosome.Menu.Interpreter.MenuFilter (interpretFilter)
import Ribosome.Menu.Interpreter.MenuStream (interpretMenuStream)
import Ribosome.Menu.Interpreter.MenuUiPure (interpretMenuUiPure)
import Ribosome.Menu.Interpreter.MenuUiWindow (interpretMenuUiWindow)
import qualified Ribosome.Menu.Prompt.Data.Prompt as Prompt
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.UpdateState (insertItems, updateQuery)

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

clampCursor ::
  MenuState s =>
  s ->
  CursorIndex ->
  (CursorIndex, CursorIndex)
clampCursor s cursor =
  (validCursor, validCursor)
  where
    validCursor = min (CursorIndex (subClamp @Word total 1)) cursor
    total = entriesLength (s ^. entries)

renderEvent ::
  MenuState s =>
  Members [MState (MS s), MState CursorIndex, MenuUi !! RpcError, Events MenuEvent, Log] r =>
  RenderEvent ->
  Sem r ()
renderEvent (RenderEvent desc anchor) = do
  Log.debug [exon|menu render: #{desc}|]
  MS s <- mread
  c <- mstate (clampCursor s)
  resume @_ @MenuUi
    do
      MenuUi.render (RenderMenu.fromState (WithCursor s c) anchor)
      publish Rendered
    do
      \ e -> Log.error [exon|menu render: #{rpcError e}|]

-- | Call the effect that hides the streaming internals by passing all actions to it so they don't need to be lowered
-- with 'withStrategicToFinal'.
--
-- - @Queue.readMaybe@ pulls 'Prompt' events emitted by 'Menu'.
--
-- - @update@ applies a new query to the current set of items.
--
-- - @insert@ adds new items to the set.
--
-- - @Queue.write@ sends 'RenderEvent's to 'renderEvent'.
--
-- - @publish@ sends 'MenuEvent's to consumers of 'Events'.
menuStream ::
  ∀ s r .
  MenuState s =>
  Members [Queue (Maybe Prompt), Queue RenderEvent, Events MenuEvent] r =>
  Members [MenuStream, MenuFilter, MState (MS s), Sync MenuSync, Log] r =>
  SerialT IO (MenuItem (Item s)) ->
  Sem r ()
menuStream items = do
  Log.debug "Starting menu stream"
  MenuStream.menuStream items Queue.readMaybe update insert Queue.write (publish MenuEvent.Exhausted)
  Log.debug "Finished menu stream"
  where
    insert new = do
      Log.debug "menu: schedule insert"
      msState (insertItems new)
      RenderEvent "new items" AnchorLine <$ publish Inserted
    update p = do
      Log.debug "menu: schedule query update"
      event <- msState (updateQuery ((.text) <$> p))
      for_ event \ qe -> publish (MenuEvent.Query qe)
      RenderEvent "query update" AnchorLine <$ Sync.putTry MenuSync

updateMenuQuery ::
  Members [Reader MenuConfig, Queue (Maybe Prompt), Sync MenuSync, Log] r =>
  Maybe Prompt ->
  Sem r ()
updateMenuQuery new = do
  Queue.write new
  whenM (asks (view #sync)) do
    Log.debug "menu: Waiting for query update"
    MenuSync <- Sync.takeBlock
    Log.debug "menu: Query update signalled"

type MenuLoopIO =
  [
    Log,
    Mask,
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
    Events MenuEvent,
    EventConsumer MenuEvent,
    Gates
  ]

interpretMenuDeps ::
  Member (Final IO) r =>
  Members MenuLoopIO r =>
  InterpretersFor MenuLoopDeps r
interpretMenuDeps =
  interpretGates .
  interpretEventsChan .
  interpretMStates .
  runReader def .
  interpretMenuStream

type NvimMenus =
  ScopedMenuUi WindowConfig : MenuFilter : MenuLoopDeps

interpretWindowMenu ::
  Members MenuLoopIO r =>
  Members [Settings !! SettingError, Rpc !! RpcError, Scratch !! RpcError, EventConsumer Event, Final IO] r =>
  InterpretersFor NvimMenus r
interpretWindowMenu =
  interpretMenuDeps .
  interpretFilter .
  interpretMenuUiWindow

type MenuScope s =
  [
    MenuUi !! RpcError,
    Gate @@ "prompt",
    MState (MS s),
    MState CursorIndex,
    Sync MenuSync,
    Queue (Maybe Prompt),
    Queue RenderEvent
  ]

menuScope ::
  MenuState s =>
  Members MenuLoopIO r =>
  Members MenuLoopDeps r =>
  Members [ScopedMenuUi ui, MenuFilter, Stop RpcError] r =>
  UiMenuParams ui s ->
  (() -> Sem (MenuScope s ++ r) a) ->
  Sem r a
menuScope (UiMenuParams (MenuParams items initial) ui) use =
  interpretQueueTBM 64 $
  interpretQueueTBM 64 $
  interpretSync $
  interpretMState def $
  interpretMState (MS initial) $
  withAsync_ (menuStream items) $
  withGate $ untag do
    restop $ scoped ui $ raiseUnder do
      withAsync_ (Queue.loop renderEvent) do
        use ()

interpretMenus ::
  ∀ ui s r .
  MenuState s =>
  Members MenuLoopIO r =>
  Members MenuLoopDeps r =>
  Members [ScopedMenuUi ui, MenuFilter] r =>
  InterpreterFor (UiMenus ui s !! RpcError) r
interpretMenus =
  interpretScopedResumableWithH @(MenuScope s) menuScope \ () -> \case
    MenuEngine (Bundle Here e) ->
      case e of
        Menu.ReadCursor ->
          pureT =<< mread
        Menu.UseCursor f ->
          muse (mstateT f)
        Menu.ReadState ->
          pureT . (.unMS) =<< mread
        Menu.UseState f -> do
          (a, modeChange) <- muse $ viaMS \ s -> do
            (newS, a) <- mstateT f s
            pure (newS, (a, s ^. mode /= newS ^. mode))
          when modeChange (updateMenuQuery Nothing)
          pure a
    MenuEngine (Bundle (There Here) e) ->
      case e of
        MenuUi.RenderPrompt consumerChange prompt -> do
          restop (MenuUi.renderPrompt consumerChange prompt)
          publish (MenuEvent.PromptUpdated prompt)
          unitT
        MenuUi.PromptEvent ->
          pureT =<< restop MenuUi.promptEvent
        MenuUi.Render m ->
          pureT =<< restop (MenuUi.render m)
        MenuUi.PromptScratch ->
          pureT =<< restop (MenuUi.promptScratch)
        MenuUi.StatusScratch ->
          pureT =<< restop (MenuUi.statusScratch)
        MenuUi.ItemsScratch ->
          pureT =<< restop (MenuUi.itemsScratch)
    MenuEngine (Bundle (There (There Here)) e) ->
      case e of
        Menu.StartPrompt ->
          pureT =<< tag @"prompt" signal
        Menu.WaitPrompt ->
          pureT =<< tag @"prompt" gate
        Menu.PromptQuit ->
          pureT =<< Queue.close
        Menu.UpdateQuery new -> do
          updateMenuQuery (Just new)
          unitT
        Menu.Render anchor -> do
          Queue.write (RenderEvent "consumer" anchor)
          unitT
        Menu.PromptLooped ->
          pureT =<< publish PromptLoop
    MenuEngine (Bundle (There (There (There e))) _) ->
      case e of

type NvimMenuIO =
  [Settings !! SettingError, Rpc !! RpcError, Scratch !! RpcError, EventConsumer Event, Final IO]

interpretSingleWindowMenu ::
  ∀ s r .
  MenuState s =>
  Members MenuLoopIO r =>
  Members NvimMenuIO r =>
  InterpretersFor (WindowMenus s !! RpcError : MenuFilter : MenuLoopDeps) r
interpretSingleWindowMenu =
  interpretMenuDeps .
  interpretFilter .
  interpretMenuUiWindow .
  interpretMenus .
  raiseUnder

promptInputFilter ::
  ∀ s r .
  MenuState s =>
  Members MenuLoopIO r =>
  Members NvimMenuIO r =>
  Member MenuFilter r =>
  [PromptEvent] ->
  InterpretersFor (WindowMenus s !! RpcError : MenuLoopDeps |> ChronosTime) r
promptInputFilter events =
  interpretTimeChronos .
  interpretMenuDeps .
  interpretMenuUiPure True events .
  interpretMenus .
  raiseUnder .
  menuSync

promptInput ::
  ∀ s r .
  MenuState s =>
  Members MenuLoopIO r =>
  Members NvimMenuIO r =>
  [PromptEvent] ->
  InterpretersFor (WindowMenus s !! RpcError : MenuLoopDeps ++ [MenuFilter, ChronosTime]) r
promptInput events =
  interpretTimeChronos .
  interpretFilter .
  interpretMenuDeps .
  interpretMenuUiPure True events .
  interpretMenus .
  raiseUnder .
  menuSync
