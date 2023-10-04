module Ribosome.Menu.Loop where

import Control.Monad.Extra (loopM)
import Exon (exon)
import qualified Log
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (SerialT)

import Ribosome.Data.Mapping (MappingLhs (MappingLhs), MappingSpec)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Menu.Action (MenuActionSem)
import qualified Ribosome.Menu.App
import Ribosome.Menu.App (InputDispatch, MenuApp, PromptApp, hoistPromptApp, promptApp)
import Ribosome.Menu.Class.MenuState (MenuState (Item))
import Ribosome.Menu.Data.InputParams (
  InputMode,
  InputParams (InputParams),
  InputTrigger (InputMapping, InputPrompt),
  describeInputMode,
  promptMode,
  )
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuResult (MenuResult)
import qualified Ribosome.Menu.Data.PromptAction as PromptAction
import Ribosome.Menu.Data.PromptAction (PromptAction)
import qualified Ribosome.Menu.Data.WindowConfig
import Ribosome.Menu.Data.WindowConfig (WindowConfig, WindowOptions, toWindowConfig)
import qualified Ribosome.Menu.Effect.Menu as Menu
import Ribosome.Menu.Effect.Menu (
  Menu,
  MenuCore,
  MenuEngine,
  MenuLoop,
  MenuParams (MenuParams),
  Menus,
  UiMenuParams (UiMenuParams),
  UiMenus,
  bundleMenuEngine,
  )
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Effect.MenuUi (MenuUi)
import qualified Ribosome.Menu.Prompt.Data.Prompt
import Ribosome.Menu.Prompt.Data.Prompt (
  Prompt,
  PromptControl (PromptControlApp, PromptControlItems),
  PromptState,
  initPrompt,
  )
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

menuAction ::
  Members [MenuCore, Reader PromptState] r =>
  MenuAction result ->
  Sem r (PromptAction (MenuResult result))
menuAction = \case
  MenuAction.Continue ->
    pure PromptAction.Continue
  MenuAction.Render anchor ->
    PromptAction.Continue <$ Menu.render anchor
  MenuAction.UpdatePrompt new -> do
    old <- ask
    pure (PromptAction.Update (old & #prompt .~ new))
  MenuAction.UpdatePromptState new ->
    pure (PromptAction.Update new)
  MenuAction.Quit result ->
    pure (PromptAction.Quit result)

mappingResult ::
  Members [MenuCore, Reader PromptState, Log] r =>
  MappingLhs ->
  MenuAction result ->
  Sem r (PromptAction (MenuResult result))
mappingResult (MappingLhs trigger) action = do
  Log.debug [exon|menu mapping #{trigger}: #{MenuAction.describe action}|]
  menuAction action

handleMapping ::
  Members [MenuCore, Reader PromptState, Log] r =>
  (InputParams -> Maybe (MenuActionSem r result)) ->
  InputMode ->
  MappingLhs ->
  Sem r (PromptAction (MenuResult result))
handleMapping dispatch inputMode trigger =
  case dispatch params of
    Just handler ->
      handler >>= \case
        Just action -> do
          Log.debug [exon|menu mapping ##{trigger}: #{MenuAction.describe action}|]
          menuAction action
        Nothing ->
          pure PromptAction.Continue
    Nothing ->
      PromptAction.Continue <$ Log.error [exon|No mapping handler for ##{trigger} in #{describeInputMode inputMode}|]
  where
    params = InputParams (InputMapping trigger) inputMode

noPromptHandlerMessage :: PromptControl -> Text
noPromptHandlerMessage = \case
  PromptControlItems -> "Menu has no default prompt handler."
  PromptControlApp -> "Menu app activated prompt control, but has no handler."

handlePromptUpdate ::
  Members [Reader PromptState, MenuCore, Log] r =>
  Maybe (MenuActionSem r result) ->
  Sem r (PromptAction (MenuResult result))
handlePromptUpdate = \case
  Just handler -> do
    prompt <- ask
    when (prompt.control == PromptControlApp) do
      Log.debug "Sending prompt update to app handler"
    handler >>= traverse menuAction <&> \case
      Just PromptAction.Continue -> PromptAction.Update prompt
      Just action -> action
      Nothing -> PromptAction.Update prompt
  Nothing -> do
    prompt <- ask
    Log.debug (noPromptHandlerMessage prompt.control)
    pure (PromptAction.Update prompt)

-- TODO what would be the problem with storing the prompt in State?
-- This has become pretty clunky.
handlePromptEvent ::
  Members [Menu s, MenuCore, Log] r =>
  InputDispatch s r result ->
  InputMode ->
  PromptState ->
  PromptEvent ->
  Sem r (PromptAction (MenuResult result))
handlePromptEvent dispatch inputMode oldState = \case
  PromptEvent.Update newPrompt -> do
    let newState = oldState & #prompt .~ newPrompt
    runReader newState $ subsume do
      handlePromptUpdate (dispatch (InputParams InputPrompt inputMode))
  PromptEvent.Mapping m ->
    runReader oldState $ subsume do
      handleMapping dispatch inputMode m
  PromptEvent.Quit Nothing ->
    pure (PromptAction.Quit MenuResult.Aborted)
  PromptEvent.Quit (Just err) ->
    pure (PromptAction.Quit (MenuResult.Error err))
  PromptEvent.Ignore ->
    pure PromptAction.Continue

consumerChangedPrompt :: Prompt -> PromptEvent -> Bool
consumerChangedPrompt new = \case
  PromptEvent.Update old -> new /= old
  _ -> True

promptAction ::
  Members [MenuCore, MenuUi] r =>
  PromptState ->
  PromptEvent ->
  PromptAction (MenuResult result) ->
  Sem r (Either PromptState (MenuResult result))
promptAction old event = \case
  PromptAction.Quit result -> do
    Menu.promptQuit
    pure (Right result)
  PromptAction.Update new -> do
    unless (old == new) do
      MenuUi.renderPrompt (consumerChangedPrompt new.prompt event) new.prompt
      when (new.control == PromptControlItems) do
        Menu.updateQuery new.prompt
    pure (Left new)
  PromptAction.Continue ->
    pure (Left old)

menuStep ::
  Member Log r =>
  Members (MenuLoop s) r =>
  InputDispatch s r result ->
  PromptState ->
  Sem r (Either PromptState (MenuResult result))
menuStep dispatch old = do
  event <- MenuUi.promptEvent
  Log.debug [exon|prompt: #{show event}|]
  action <- handlePromptEvent dispatch (promptMode old) old event
  promptAction old event action <* Menu.promptLooped

menuLoop' ::
  ∀ s result r .
  Member Log r =>
  Members (MenuLoop s) r =>
  PromptState ->
  InputDispatch s r result ->
  Sem r (MenuResult result)
menuLoop' promptConf dispatch = do
  Log.debug "Starting prompt loop"
  MenuUi.renderPrompt True prompt.prompt
  Menu.startPrompt
  Menu.updateQuery prompt.prompt
  loopM (menuStep (fmap (insertAt @2) . dispatch)) prompt
  where
    prompt = initPrompt promptConf

menuLoop ::
  ∀ s result r .
  Member Log r =>
  Members (MenuLoop s) r =>
  PromptApp s r result ->
  Sem r (MenuResult result)
menuLoop app =
  menuLoop' app.prompt app.dispatch

menuLoopEngine ::
  ∀ s result r .
  Member Log r =>
  Member (MenuEngine s) r =>
  PromptState ->
  InputDispatch s r result ->
  Sem r (MenuResult result)
menuLoopEngine prompt dispatch =
  bundleMenuEngine (menuLoop' prompt (fmap (insertAt @2) . dispatch))

menuAppWith ::
  MenuState s =>
  Bool ->
  Bool ->
  PromptState ->
  MenuApp s r result ->
  (PromptApp s r result -> Sem r' a) ->
  Sem r' a
menuAppWith addBuiltin addDefault prompt app use =
  use (promptApp addBuiltin addDefault prompt app)

menuApp ::
  MenuState s =>
  PromptState ->
  MenuApp s r result ->
  (PromptApp s r result -> Sem r' a) ->
  Sem r' a
menuApp =
  menuAppWith True True

menuParamsEngine ::
  Member (Menus s) r =>
  SerialT IO (MenuItem (Item s)) ->
  s ->
  InterpreterFor (MenuEngine s) r
menuParamsEngine items initial =
  scoped (MenuParams items initial)

menuParams ::
  Member (Menus s) r =>
  SerialT IO (MenuItem (Item s)) ->
  s ->
  InterpretersFor (MenuLoop s) r
menuParams items initial =
  menuParamsEngine items initial .
  bundleMenuEngine .
  raise3Under

runPromptApp ::
  ∀ result s r .
  Members [Menus s, Log] r =>
  SerialT IO (MenuItem (Item s)) ->
  s ->
  PromptApp s r result ->
  Sem r (MenuResult result)
runPromptApp items initial app =
  menuParams items initial do
    menuLoop (hoistPromptApp (insertAt @2) app)

withUi' ::
  Member (UiMenus ui s) r =>
  ui ->
  InterpreterFor (Menus s) r
withUi' ui =
  rescope \ p -> UiMenuParams p ui

withUi ::
  Members [UiMenus ui s !! RpcError, Stop RpcError] r =>
  ui ->
  InterpreterFor (Menus s) r
withUi ui =
  restop .
  withUi' ui .
  raiseUnder

headlessMenu ::
  ∀ result s r .
  MenuState s =>
  Members [Menus s !! RpcError, Log, Stop RpcError] r =>
  SerialT IO (MenuItem (Item s)) ->
  s ->
  PromptState ->
  MenuApp s r result ->
  Sem r (MenuResult result)
headlessMenu items initial prompt app =
  menuApp prompt app \ papp ->
    restop @RpcError $ runPromptApp items initial (hoistPromptApp raise2Under papp)

uiMenuApp ::
  ∀ result s ui r a .
  MenuState s =>
  Members [UiMenus ui s !! RpcError, Log, Stop RpcError] r =>
  SerialT IO (MenuItem (Item s)) ->
  s ->
  PromptState ->
  ([MappingSpec] -> ui) ->
  MenuApp s r result ->
  (PromptApp s (MenuLoop s ++ r) result -> Sem (MenuLoop s ++ r) a) ->
  Sem r a
uiMenuApp items initial prompt ui app use =
  menuApp prompt app \ papp ->
    withUi (ui papp.mappings) do
      menuParams items initial do
        raise3Under (use (hoistPromptApp (insertAt @2) papp))

uiMenu ::
  ∀ result s ui r .
  MenuState s =>
  Members [UiMenus ui s !! RpcError, Log, Stop RpcError] r =>
  SerialT IO (MenuItem (Item s)) ->
  s ->
  PromptState ->
  ([MappingSpec] -> ui) ->
  MenuApp s r result ->
  Sem r (MenuResult result)
uiMenu items initial prompt ui app =
  uiMenuApp items initial prompt ui app \ papp ->
    menuLoop (hoistPromptApp (insertAt @2) papp)

windowMenuApp ::
  ∀ result s r a .
  MenuState s =>
  Members [UiMenus WindowConfig s !! RpcError, Log, Stop RpcError] r =>
  SerialT IO (MenuItem (Item s)) ->
  s ->
  WindowOptions ->
  MenuApp s r result ->
  (PromptApp s (MenuLoop s ++ r) result -> Sem (MenuLoop s ++ r) a) ->
  Sem r a
windowMenuApp items initial options =
  uiMenuApp items initial options.prompt (toWindowConfig options)

windowMenu ::
  ∀ result s r .
  MenuState s =>
  Members [UiMenus WindowConfig s !! RpcError, Log, Stop RpcError] r =>
  SerialT IO (MenuItem (Item s)) ->
  s ->
  WindowOptions ->
  MenuApp s r result ->
  Sem r (MenuResult result)
windowMenu items initial options app =
  windowMenuApp items initial options app \ papp ->
    menuLoop (hoistPromptApp (insertAt @2) papp)

staticWindowMenu ::
  ∀ result s r .
  MenuState s =>
  Members [UiMenus WindowConfig s !! RpcError, Log, Stop RpcError] r =>
  [MenuItem (Item s)] ->
  s ->
  WindowOptions ->
  MenuApp s r result ->
  Sem r (MenuResult result)
staticWindowMenu items =
  windowMenu (Stream.fromList items)
