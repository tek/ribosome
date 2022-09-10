module Ribosome.Menu.Loop where

import Conc (PScoped, pscoped)
import Control.Monad.Extra (loopM)
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
import Ribosome.Menu.Interpreter.MenuState (mstateT)
import Ribosome.Menu.Mappings (Mappings, defaultMappings)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

interpretMenuStateMenuLoop ::
  Member (MenuLoop f i) r =>
  InterpreterFor (MenuState f i) r
interpretMenuStateMenuLoop =
  interpretH \case
    MenuState.ReadCursor ->
      pureT =<< MenuLoop.readCursor
    MenuState.UseCursor f ->
      MenuLoop.useCursor (mstateT f)
    MenuState.ReadItems ->
      pureT =<< MenuLoop.readItems
    MenuState.UseItems f ->
      MenuLoop.useItems (mstateT f)

menuAction ::
  Member (MenuLoop f i) r =>
  MenuAction f a ->
  Sem r (PromptAction f (MenuResult a))
menuAction = \case
  MenuAction.Continue ->
    pure PromptAction.Continue
  MenuAction.Render ->
    PromptAction.Continue <$ MenuLoop.render
  MenuAction.UpdatePrompt prompt ->
    pure (PromptAction.Update prompt)
  MenuAction.ChangeFilter f ->
    pure (PromptAction.ChangeFilter f)
  MenuAction.Quit result ->
    pure (PromptAction.Quit result)

mappingResult ::
  Show f =>
  Members [MenuLoop f i, Log] r =>
  Text ->
  MenuAction f a ->
  Sem r (PromptAction f (MenuResult a))
mappingResult trigger action = do
  Log.debug [exon|menu mapping #{trigger}: #{MenuAction.describe action}|]
  menuAction action

handlePromptEvent ::
  Show f =>
  Members [MenuLoop f i, Log] r =>
  (MappingLhs -> Maybe (Sem r (Maybe (MenuAction f a)))) ->
  PromptEvent ->
  Sem r (PromptAction f (MenuResult a))
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
  ∀ f i a r .
  Show f =>
  Members [MenuLoop f i, MenuUi, Log] r =>
  (MappingLhs -> Maybe (MenuWidget f i r a)) ->
  Sem r (MenuResult a)
menuLoop mappings =
  MenuLoop.withRender MenuUi.render do
    Log.debug "Starting prompt loop"
    MenuLoop.startPrompt
    flip loopM def \ old -> do
      event <- MenuUi.promptEvent old
      Log.debug [exon|prompt: #{show event}|]
      action <- runReader old (interpretMenuStateMenuLoop (handlePromptEvent mappings event)) >>= \case
        PromptAction.Quit result -> do
          MenuLoop.promptQuit
          pure (Right result)
        PromptAction.Update new -> do
          MenuLoop.promptUpdated new
          MenuUi.renderPrompt (consumerChangedPrompt new event) new
          pure (Left new)
        PromptAction.ChangeFilter f -> do
          MenuLoop.changeFilter f
          pure (Left old)
        PromptAction.Continue ->
          pure (Left old)
      MenuLoop.promptLooped
      pure action

lookupMapping ::
  Mappings f i r result ->
  MappingLhs ->
  Maybe (MenuWidget f i r result)
lookupMapping mappings i =
  Map.lookup i byLhs
  where
    byLhs =
      Map.mapKeys Mapping.lhs mappings

menuMaps ::
  ∀ f i result r .
  Show f =>
  Members [MenuLoop f i, MenuUi, Log] r =>
  Mappings f i r result ->
  Sem r (MenuResult result)
menuMaps mappings =
  menuLoop (lookupMapping mappings)

runMenu ::
  Member (MenuLoops f i) r =>
  SerialT IO (MenuItem i) ->
  f ->
  InterpreterFor (MenuLoop f i) r
runMenu items initialFilter =
  pscoped (items, initialFilter)

menu ::
  ∀ f i a r .
  Show f =>
  Members [MenuLoops f i, MenuUi, Log] r =>
  SerialT IO (MenuItem i) ->
  f ->
  Mappings f i r a ->
  Sem r (MenuResult a)
menu items initialFilter (fmap raise2Under -> mappings) =
  runMenu items initialFilter (menuMaps mappings)

nvimMenuLoop ::
  ∀ mres result f i r .
  Show f =>
  Members [NvimMenuUi mres, MenuLoop f i, Log, Stop RpcError] r =>
  PromptConfig ->
  ScratchOptions ->
  Mappings f i r result ->
  Sem r (MenuResult result)
nvimMenuLoop pconf options consumerMappings =
  restop @_ @(PScoped _ _ MenuUi) $ withMenuUi (NvimMenuConfig pconf options maps) do
    menuMaps (insertAt @2 <$> mappings)
  where
    maps =
      Map.keys mappings
    mappings =
      defaultMappings <> consumerMappings

nvimMenu ::
  ∀ mres result f i r .
  Show f =>
  Members [NvimMenuUi mres, MenuLoops f i, Log, Stop RpcError] r =>
  SerialT IO (MenuItem i) ->
  f ->
  PromptConfig ->
  ScratchOptions ->
  Mappings f i r result ->
  Sem r (MenuResult result)
nvimMenu items initialFilter pconf options consumerMappings =
  restop @_ @(PScoped _ _ MenuUi) $ withMenuUi (NvimMenuConfig pconf options maps) do
    menu items initialFilter (insertAt @2 <$> mappings)
  where
    maps =
      Map.keys mappings
    mappings =
      defaultMappings <> consumerMappings

staticNvimMenu ::
  ∀ mres result f i r .
  Show f =>
  Members [NvimMenuUi mres, MenuLoops f i, Log, Stop RpcError] r =>
  [MenuItem i] ->
  f ->
  PromptConfig ->
  ScratchOptions ->
  Mappings f i r result ->
  Sem r (MenuResult result)
staticNvimMenu items =
  nvimMenu (Stream.fromList items)
