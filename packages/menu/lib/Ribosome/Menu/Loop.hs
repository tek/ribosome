module Ribosome.Menu.Loop where

import Control.Monad.Extra (loopM)
import qualified Data.Map.Strict as Map
import Exon (exon)
import qualified Log
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (SerialT)

import qualified Ribosome.Data.Mapping as Mapping
import Ribosome.Data.Mapping (MappingLhs (MappingLhs))
import Ribosome.Host.Data.Report (ReportLog, Reportable)
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
import Ribosome.Menu.Data.WindowConfig (WindowOptions, toWindowConfig)
import qualified Ribosome.Menu.Effect.Menu as Menu
import Ribosome.Menu.Effect.Menu (Menu, Menus)
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Effect.MenuUi (MenuUi, NvimMenuUi, withMenuUi)
import Ribosome.Menu.Mappings (Mappings, defaultMappings)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Report (logReport)

menuAction ::
  Member (Menu s) r =>
  MenuAction a ->
  Sem r (PromptAction (MenuResult a))
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
  Members [Menu s, Log] r =>
  Text ->
  MenuAction a ->
  Sem r (PromptAction (MenuResult a))
mappingResult trigger action = do
  Log.debug [exon|menu mapping #{trigger}: #{MenuAction.describe action}|]
  menuAction action

handlePromptEvent ::
  Members [Menu s, Log] r =>
  (MappingLhs -> Maybe (Sem r (Maybe (MenuAction a)))) ->
  PromptEvent ->
  Sem r (PromptAction (MenuResult a))
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

menuLoop ::
  ∀ s e result r .
  Reportable e =>
  Members [Menu s, MenuUi !! e, Log, ReportLog, Stop e] r =>
  (MappingLhs -> Maybe (MenuWidget s r result)) ->
  Sem r (MenuResult result)
menuLoop mappings =
  Menu.withRender (resuming logReport . MenuUi.render) do
    Log.debug "Starting prompt loop"
    Menu.startPrompt
    flip loopM def \ old -> do
      event <- restop (MenuUi.promptEvent old)
      Log.debug [exon|prompt: #{show event}|]
      action <- runReader old (subsume (handlePromptEvent mappings event)) >>= \case
        PromptAction.Quit result -> do
          Menu.promptQuit
          pure (Right result)
        PromptAction.Update new -> do
          Menu.promptUpdated new
          restop (MenuUi.renderPrompt (consumerChangedPrompt new event) new)
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

menuMaps ::
  ∀ s result e r .
  Reportable e =>
  Members [Menu s, MenuUi !! e, Log, ReportLog, Stop e] r =>
  Mappings s r result ->
  Sem r (MenuResult result)
menuMaps mappings =
  menuLoop (lookupMapping mappings)

runMenu ::
  Member (Menus s) r =>
  SerialT IO (MenuItem (Item s)) ->
  s ->
  InterpreterFor (Menu s) r
runMenu items initial =
  scoped (items, initial)

menu ::
  ∀ s e a r .
  Reportable e =>
  Members [Menus s, MenuUi !! e, Log, ReportLog, Stop e] r =>
  SerialT IO (MenuItem (Item s)) ->
  s ->
  Mappings s r a ->
  Sem r (MenuResult a)
menu items initial (fmap raise2Under -> mappings) =
  runMenu items initial (menuMaps mappings)

nvimMenuLoop ::
  ∀ mres result s r .
  MenuState s =>
  Members [NvimMenuUi mres, Menu s, Log, ReportLog, Stop RpcError] r =>
  WindowOptions ->
  Mappings s r result ->
  Sem r (MenuResult result)
nvimMenuLoop options consumerMappings =
  restop @_ @(Scoped _ _ (MenuUi !! RpcError)) $ withMenuUi (toWindowConfig options mappings) do
    menuMaps (insertAt @2 <$> mappings)
  where
    mappings =
      defaultMappings <> consumerMappings

nvimMenu ::
  ∀ mres result s r .
  MenuState s =>
  Members [NvimMenuUi mres, Menus s, Log, ReportLog, Stop RpcError] r =>
  SerialT IO (MenuItem (Item s)) ->
  s ->
  WindowOptions ->
  Mappings s r result ->
  Sem r (MenuResult result)
nvimMenu items initial options consumerMappings =
  restop @_ @(Scoped _ _ (MenuUi !! RpcError)) $ withMenuUi (toWindowConfig options mappings) do
    menu items initial (insertAt @2 <$> mappings)
  where
    mappings =
      defaultMappings <> consumerMappings

staticNvimMenu ::
  ∀ mres result s r .
  MenuState s =>
  Members [NvimMenuUi mres, Menus s, Log, ReportLog, Stop RpcError] r =>
  [MenuItem (Item s)] ->
  s ->
  WindowOptions ->
  Mappings s r result ->
  Sem r (MenuResult result)
staticNvimMenu items =
  nvimMenu (Stream.fromList items)
