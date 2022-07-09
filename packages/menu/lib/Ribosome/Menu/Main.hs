module Ribosome.Menu.Main where

import Data.Generics.Labels ()
import Exon (exon)
import qualified Polysemy.Log as Log
import Prelude hiding (consume)
import Streamly.Prelude (SerialT)

import Ribosome.Menu.Data.MenuAction (MenuAction)
import qualified Ribosome.Menu.Data.MenuAction as MenuAction (MenuAction (..))
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Data.MenuState (semState)
import qualified Ribosome.Menu.Data.QuitReason as QuitReason
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer, menuConsumerEvent)
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import qualified Ribosome.Menu.Effect.MenuRenderer as MenuRenderer
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer, withMenuRenderer)
import Ribosome.Menu.Effect.MenuState (MenuState, readMenu, useItems)
import qualified Ribosome.Menu.Effect.MenuStream as MenuStream
import Ribosome.Menu.Effect.MenuStream (MenuStream)
import Ribosome.Menu.Effect.PromptControl (PromptControl, sendControlEvent)
import Ribosome.Menu.Effect.PromptInput (PromptInput)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer, withPrompt)
import Ribosome.Menu.Effect.PromptStream (PromptStream)
import Ribosome.Menu.Interpreter.MenuState (MenuIOStack, MenuStack, interpretMenuFinal)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import qualified Ribosome.Menu.Prompt.Data.PromptControlEvent as PromptControlEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Prompt.Data.PromptFlag (PromptFlag)
import Ribosome.Menu.Prompt.Run (PromptStack, promptEventStream)
import Ribosome.Menu.UpdateState (insertItems, queryUpdate, setPromptAndClassify)

eventAction ::
  MenuEvent ->
  MenuAction a
eventAction = \case
  MenuEvent.Init ->
    MenuAction.Continue
  MenuEvent.PromptEdit ->
    MenuAction.Render
  MenuEvent.PromptNavigation ->
    MenuAction.Render
  MenuEvent.Mapping _ ->
    MenuAction.Continue
  MenuEvent.NewItems ->
    MenuAction.Render
  MenuEvent.Exhausted ->
    MenuAction.Continue
  MenuEvent.Quit QuitReason.Aborted ->
    MenuAction.Quit MenuResult.Aborted
  MenuEvent.Quit (QuitReason.Error msg) ->
    MenuAction.Quit (MenuResult.Error msg)

outputAction ::
  Member PromptControl r =>
  MenuAction a ->
  Sem r (Maybe (MenuResult a))
outputAction = \case
  MenuAction.Continue ->
    pure Nothing
  MenuAction.Render ->
    pure Nothing
  MenuAction.UpdatePrompt prompt ->
    Nothing <$ sendControlEvent (PromptControlEvent.Set prompt)
  MenuAction.Quit result ->
    Just result <$ sendControlEvent PromptControlEvent.Quit

renderAction ::
  Members [MenuState i, MenuRenderer i, Log] r =>
  MenuAction a ->
  Sem r ()
renderAction = \case
  MenuAction.Render ->
    MenuRenderer.menuRender =<< readMenu
  _ ->
    unit

sendQuit ::
  Member PromptControl r =>
  Sem r ()
sendQuit =
  sendControlEvent PromptControlEvent.Quit

menuStream ::
  ∀ i r a .
  Show a =>
  Member (MenuStream i) r =>
  Members [MenuState i, PromptControl, MenuRenderer i, MenuConsumer a, MenuFilter, Log] r =>
  SerialT IO (MenuItem i) ->
  SerialT IO (Prompt, PromptEvent) ->
  Sem r (Maybe (MenuResult a))
menuStream items promptEvents =
  MenuStream.menuStream items promptEvents handleAction setPromptAndClassify queryUpdate insert
  sendQuit consume outputAction
  where
    handleAction action = do
      Log.debug [exon|menu consumer: #{show action}|]
      renderAction action
    insert new =
      MenuEvent.NewItems <$ useItems \ its -> runState its (semState (insertItems new))
    consume event = do
      menuAction <- menuConsumerEvent event
      pure (fromMaybe (eventAction event) menuAction)

menuResult ::
  Member Log r =>
  Maybe (MenuResult a) ->
  Sem r (MenuResult a)
menuResult = \case
  Just result ->
    result <$ Log.debug [exon|menu terminated: #{describe result}|]
  Nothing -> do
    Log.debug "menu terminated without output"
    pure MenuResult.NoAction
  where
    describe = \case
      MenuResult.Success _ -> "success"
      MenuResult.Error msg -> msg
      MenuResult.Aborted -> "user interrupt"
      MenuResult.NoAction -> "no action"

menuMain ::
  Show a =>
  Members PromptStack r =>
  Member (Reader (MenuConfig i)) r =>
  Members [MenuStream i, PromptStream, MenuState i, MenuConsumer a, MenuRenderer i, MenuFilter] r =>
  Member Log r =>
  Sem r (MenuResult a)
menuMain = do
  MenuConfig items <- ask
  promptEvents <- promptEventStream
  menuResult =<< menuStream items promptEvents

simpleMenu ::
  Show a =>
  Members [Log, Mask mres, Resource, Race, Embed IO, Final IO] r =>
  Members [Scoped pres PromptRenderer, PromptInput, MenuRenderer i] r =>
  InterpreterFor (MenuConsumer a) (MenuIOStack i ++ PromptRenderer : r) ->
  MenuConfig i ->
  [PromptFlag] ->
  Bool ->
  Sem r (MenuResult a)
simpleMenu consumer config flags monotonic =
  withPrompt do
    interpretMenuFinal config flags monotonic $ consumer $ menuMain

menu ::
  ∀ a i pres mrres r .
  Show a =>
  Members (MenuStack i) r =>
  Members [MenuStream i, PromptStream, MenuConsumer a, PromptInput, Scoped pres PromptRenderer] r =>
  Members [Scoped mrres (MenuRenderer i), Log] r =>
  Sem r (MenuResult a)
menu =
  withMenuRenderer $ withPrompt do
    menuMain
