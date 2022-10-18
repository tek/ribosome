module Ribosome.Menu.Loop where

import Conc (rescope)
import Control.Monad.Extra (loopM)
import qualified Data.Map.Strict as Map
import Exon (exon)
import qualified Log
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (SerialT)

import qualified Ribosome.Data.Mapping as Mapping
import Ribosome.Data.Mapping (MappingLhs (MappingLhs))
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Menu.Action (MenuWidget)
import Ribosome.Menu.Class.MenuState (MenuState (Item))
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuResult (MenuResult)
import qualified Ribosome.Menu.Data.PromptAction as PromptAction
import Ribosome.Menu.Data.PromptAction (PromptAction)
import Ribosome.Menu.Data.WindowConfig (WindowConfig, WindowOptions, toWindowConfig)
import qualified Ribosome.Menu.Effect.Menu as Menu
import Ribosome.Menu.Effect.Menu (
  MenuCore,
  MenuEngine,
  MenuEngineStack,
  MenuParams (MenuParams),
  Menus,
  UiMenuParams (UiMenuParams),
  UiMenus,
  bundleMenuEngine,
  )
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Mappings (Mappings, defaultMappings)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

menuAction ::
  Member MenuCore r =>
  MenuAction result ->
  Sem r (PromptAction (MenuResult result))
menuAction = \case
  MenuAction.Continue ->
    pure PromptAction.Continue
  MenuAction.Render ->
    PromptAction.Continue <$ Menu.render
  MenuAction.UpdatePrompt prompt ->
    pure (PromptAction.Update prompt)
  MenuAction.Quit result ->
    pure (PromptAction.Quit result)

mappingResult ::
  Members [MenuCore, Log] r =>
  Text ->
  MenuAction result ->
  Sem r (PromptAction (MenuResult result))
mappingResult trigger action = do
  Log.debug [exon|menu mapping #{trigger}: #{MenuAction.describe action}|]
  menuAction action

handlePromptEvent ::
  Members [MenuCore, Log] r =>
  (MappingLhs -> Maybe (Sem r (Maybe (MenuAction result)))) ->
  PromptEvent ->
  Sem r (PromptAction (MenuResult result))
handlePromptEvent mappings = \case
  PromptEvent.Update prompt ->
    pure (PromptAction.Update prompt)
  PromptEvent.Mapping m ->
    case mappings (MappingLhs m) of
      Just h ->
        fromMaybe PromptAction.Continue <$> (traverse (mappingResult m) =<< h)
      Nothing ->
        PromptAction.Continue <$ Log.debug [exon|No mapping handler for #{m}|]
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

menuStep ::
  Member Log r =>
  Members (MenuEngineStack s) r =>
  (MappingLhs -> Maybe (MenuWidget s r result)) ->
  Prompt ->
  Sem r (Either Prompt (MenuResult result))
menuStep mappings old = do
  event <- MenuUi.promptEvent
  Log.debug [exon|prompt: #{show event}|]
  action <- runReader old (subsume (handlePromptEvent mappings event)) >>= \case
    PromptAction.Quit result -> do
      Menu.promptQuit
      pure (Right result)
    PromptAction.Update new -> do
      Menu.promptUpdated new
      MenuUi.renderPrompt (consumerChangedPrompt new event) new
      pure (Left new)
    PromptAction.Continue ->
      pure (Left old)
  Menu.promptLooped
  pure action

lookupMapping ::
  Mappings s r result ->
  MappingLhs ->
  Maybe (MenuWidget s r result)
lookupMapping mappings i =
  Map.lookup i byLhs
  where
    byLhs =
      Map.mapKeys Mapping.lhs mappings

menuLoop' ::
  ∀ s result r .
  Members (MenuEngineStack s) r =>
  Member Log r =>
  (MappingLhs -> Maybe (MenuWidget s r result)) ->
  Sem r (MenuResult result)
menuLoop' mappings = do
  Log.debug "Starting prompt loop"
  Menu.startPrompt
  flip loopM def (menuStep (fmap (insertAt @2) . mappings))

menuLoop ::
  ∀ s result r .
  Members [MenuEngine s, Log] r =>
  (MappingLhs -> Maybe (MenuWidget s r result)) ->
  Sem r (MenuResult result)
menuLoop mappings =
  bundleMenuEngine (menuLoop' (fmap (insertAt @2) . mappings))

menuMaps ::
  ∀ s result r .
  Members [MenuEngine s, Log] r =>
  Mappings s r result ->
  Sem r (MenuResult result)
menuMaps mappings =
  menuLoop (fmap (insertAt @2) . lookupMapping mappings)

runMenu ::
  ∀ s r .
  Member (Menus s) r =>
  SerialT IO (MenuItem (Item s)) ->
  s ->
  InterpreterFor (MenuEngine s) r
runMenu items initial =
  scoped (MenuParams items initial)

runMenuUi' ::
  Member (UiMenus ui s) r =>
  SerialT IO (MenuItem (Item s)) ->
  s ->
  InterpreterFor (Scoped ui (MenuEngine s)) r
runMenuUi' items initial =
  rescope (UiMenuParams (MenuParams items initial))

runMenuUi ::
  Members [UiMenus ui s !! RpcError, Stop RpcError] r =>
  SerialT IO (MenuItem (Item s)) ->
  s ->
  InterpreterFor (Scoped ui (MenuEngine s)) r
runMenuUi items initial =
  restop .
  runMenuUi' items initial .
  raiseUnder

addMenuUi ::
  ∀ s ui r .
  Member (UiMenus ui s) r =>
  ui ->
  InterpreterFor (Menus s) r
addMenuUi ui =
  rescope (flip UiMenuParams ui)

withDefaultMappings ::
  MenuState s =>
  (Mappings s r result -> a) ->
  Mappings s r result ->
  a
withDefaultMappings f consumerMappings =
  f (defaultMappings <> consumerMappings)

withMenuUi ::
  ∀ result s r a .
  MenuState s =>
  Member (Scoped WindowConfig (MenuEngine s)) r =>
  WindowOptions ->
  (Mappings s r result -> Sem (MenuEngine s : r) a) ->
  Mappings s r result ->
  Sem r a
withMenuUi options use =
  withDefaultMappings \ mappings ->
    scoped (toWindowConfig options mappings) do
      use mappings

menu ::
  ∀ result s ui r .
  Members [UiMenus ui s !! RpcError, Log, Stop RpcError] r =>
  SerialT IO (MenuItem (Item s)) ->
  s ->
  ui ->
  Mappings s r result ->
  Sem r (MenuResult result)
menu items initial ui =
  restop .
  runMenuUi' items initial .
  raiseUnder .
  scoped ui .
  menuMaps .
  fmap (insertAt @2)

windowMenu ::
  ∀ result s r .
  MenuState s =>
  Members [UiMenus WindowConfig s !! RpcError, Log, Stop RpcError] r =>
  SerialT IO (MenuItem (Item s)) ->
  s ->
  WindowOptions ->
  Mappings s r result ->
  Sem r (MenuResult result)
windowMenu items initial options =
  restop .
  runMenuUi' items initial .
  raiseUnder .
  withMenuUi options (menuMaps . fmap (insertAt @2)) .
  fmap (insertAt @2)

staticWindowMenu ::
  ∀ result s r .
  MenuState s =>
  Members [UiMenus WindowConfig s !! RpcError, Log, Stop RpcError] r =>
  [MenuItem (Item s)] ->
  s ->
  WindowOptions ->
  Mappings s r result ->
  Sem r (MenuResult result)
staticWindowMenu items =
  windowMenu (Stream.fromList items)
