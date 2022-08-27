module Ribosome.Menu.Loop where

import Conc (PScoped, pscoped)
import qualified Data.Map.Strict as Map
import Exon (exon)
import qualified Log
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (SerialT)

import qualified Ribosome.Data.Mapping as Mapping
import Ribosome.Data.Mapping (MappingLhs (MappingLhs))
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Menu.Action (MenuWidget)
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuResult (MenuResult)
import qualified Ribosome.Menu.Data.PromptAction as PromptAction
import Ribosome.Menu.Data.PromptAction (PromptAction)
import qualified Ribosome.Menu.Effect.MenuLoop as MenuLoop
import Ribosome.Menu.Effect.MenuLoop (MenuLoop, MenuLoops)
import qualified Ribosome.Menu.Effect.MenuState as MenuState
import Ribosome.Menu.Effect.MenuState (MenuState)
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Effect.MenuUi (MenuUi, NvimMenuConfig (NvimMenuConfig), NvimMenuUi, withMenuUi)
import Ribosome.Menu.Interpreter.MenuState (mcState')
import Ribosome.Menu.Mappings (Mappings, defaultMappings)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

interpretMenuStateMenuLoop ::
  Member (MenuLoop i) r =>
  InterpreterFor (MenuState i) r
interpretMenuStateMenuLoop =
  interpretH \case
    MenuState.ReadCursor ->
      pureT =<< MenuLoop.readCursor
    MenuState.UseCursor f ->
      MenuLoop.useCursor (mcState' f)
    MenuState.ReadItems ->
      pureT =<< MenuLoop.readItems
    MenuState.UseItems f ->
      MenuLoop.useItems (mcState' f)

menuAction ::
  Member (MenuLoop i) r =>
  MenuAction a ->
  Sem r (PromptAction (MenuResult a))
menuAction = \case
  MenuAction.Continue ->
    pure PromptAction.Continue
  MenuAction.Render ->
    PromptAction.Continue <$ MenuLoop.render
  MenuAction.UpdatePrompt prompt ->
    pure (PromptAction.Update prompt)
  MenuAction.Quit result ->
    pure (PromptAction.Quit result)

mappingResult ::
  Members [MenuLoop i, Log] r =>
  Text ->
  MenuAction a ->
  Sem r (PromptAction (MenuResult a))
mappingResult trigger action = do
  Log.debug [exon|menu mapping #{trigger}: #{MenuAction.describe action}|]
  menuAction action

handlePromptEvent ::
  Members [MenuLoop i, Log] r =>
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

menu' ::
  ∀ i a r .
  Members [MenuLoop i, MenuUi, Log] r =>
  (MappingLhs -> Maybe (MenuWidget i r a)) ->
  Sem r (MenuResult a)
menu' mappings = do
  MenuLoop.withRender MenuUi.render do
    Log.debug "Starting prompt loop"
    MenuLoop.startPrompt
    untilJustM $ MenuLoop.usePrompt \ old -> do
      event <- MenuUi.promptEvent old
      Log.debug [exon|prompt: #{show event}|]
      action <- runReader old (interpretMenuStateMenuLoop (handlePromptEvent mappings event)) >>= \case
        PromptAction.Quit result -> do
          MenuLoop.promptQuit
          pure (old, Just result)
        PromptAction.Update new -> do
          MenuLoop.promptUpdated new
          MenuUi.renderPrompt (consumerChangedPrompt new event) new
          pure (new, Nothing)
        PromptAction.Continue ->
          pure (old, Nothing)
      MenuLoop.promptLooped
      pure action

lookupMapping ::
  Mappings i r result ->
  MappingLhs ->
  Maybe (MenuWidget i r result)
lookupMapping mappings i =
  Map.lookup i byLhs
  where
    byLhs =
      Map.mapKeys Mapping.lhs mappings

menuMaps ::
  ∀ i result r .
  Members [MenuLoop i, MenuUi, Log] r =>
  Mappings i r result ->
  Sem r (MenuResult result)
menuMaps mappings =
  menu' (lookupMapping mappings)

runMenu ::
  Member (MenuLoops i) r =>
  SerialT IO (MenuItem i) ->
  InterpreterFor (MenuLoop i) r
runMenu =
  pscoped

menu ::
  ∀ i a r .
  Members [MenuLoops i, MenuUi, Log] r =>
  SerialT IO (MenuItem i) ->
  Mappings i r a ->
  Sem r (MenuResult a)
menu items (fmap raise2Under -> mappings) = do
  runMenu items (menuMaps mappings)

nvimMenu' ::
  ∀ mres result i r .
  Members [NvimMenuUi mres, MenuLoop i, Log, Stop RpcError] r =>
  PromptConfig ->
  ScratchOptions ->
  Mappings i r result ->
  Sem r (MenuResult result)
nvimMenu' pconf options consumerMappings =
  restop @_ @(PScoped _ _ MenuUi) $ withMenuUi (NvimMenuConfig pconf options maps) do
    menuMaps (insertAt @2 <$> mappings)
  where
    maps =
      Map.keys mappings
    mappings =
      defaultMappings <> consumerMappings

nvimMenu ::
  ∀ mres result i r .
  Members [NvimMenuUi mres, MenuLoops i, Log, Stop RpcError] r =>
  SerialT IO (MenuItem i) ->
  PromptConfig ->
  ScratchOptions ->
  Mappings i r result ->
  Sem r (MenuResult result)
nvimMenu items pconf options consumerMappings =
  restop @_ @(PScoped _ _ MenuUi) $ withMenuUi (NvimMenuConfig pconf options maps) do
    menu items (insertAt @2 <$> mappings)
  where
    maps =
      Map.keys mappings
    mappings =
      defaultMappings <> consumerMappings

staticNvimMenu ::
  ∀ mres result i r .
  Members [NvimMenuUi mres, MenuLoops i, Log, Stop RpcError] r =>
  [MenuItem i] ->
  PromptConfig ->
  ScratchOptions ->
  Mappings i r result ->
  Sem r (MenuResult result)
staticNvimMenu items =
  nvimMenu (Stream.fromList items)
