module Ribosome.Menu.Main where

import Data.Generics.Labels ()
import Exon (exon)
import qualified Polysemy.Log as Log
import Prelude hiding (consume)
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (AsyncT, SerialT)

import Ribosome.Final (inFinal_)
import Ribosome.Menu.Data.MenuAction (MenuAction)
import qualified Ribosome.Menu.Data.MenuAction as MenuAction (MenuAction (..))
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuResult (MenuResult)
import qualified Ribosome.Menu.Data.QuitReason as QuitReason
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer, menuConsumerEvent)
import qualified Ribosome.Menu.Effect.MenuRenderer as MenuRenderer
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer, withMenuRenderer)
import Ribosome.Menu.Effect.MenuState (MenuState, readMenu)
import Ribosome.Menu.Effect.PromptControl (PromptControl, sendControlEvent)
import Ribosome.Menu.Effect.PromptEvents (PromptEvents)
import Ribosome.Menu.Effect.PromptInput (PromptInput)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer, withPrompt)
import Ribosome.Menu.Effect.PromptState (PromptState)
import Ribosome.Menu.Filters (fuzzyMonotonic)
import Ribosome.Menu.Interpreter.MenuState (MenuStack, interpretMenu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import qualified Ribosome.Menu.Prompt.Data.PromptControlEvent as PromptControlEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Prompt.Data.PromptFlag (PromptFlag)
import Ribosome.Menu.Prompt.Run (PromptStack, withPromptStream)
import Ribosome.Menu.UpdateState (promptEvent, updateItems)

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

menuStream ::
  ∀ i r a .
  Show a =>
  Member (Reader (MenuConfig i)) r =>
  Members [MenuState i, PromptControl, MenuRenderer i, MenuConsumer a, Resource, Log, Embed IO, Final IO] r =>
  AsyncT IO (Prompt, PromptEvent) ->
  Sem r (SerialT IO (MenuResult a))
menuStream promptEvents = do
  (MenuConfig items customFilter) <- ask
  inFinal_ \ lowerMaybe lower_ pur ->
    let
      handleAction action =
        lower_ do
          Log.debug [exon|menu consumer: #{show action}|]
          renderAction action
      prompt =
        promptEvent lowerMaybe itemFilter promptEvents
      menuItems =
        updateItems lowerMaybe itemFilter (Stream.fromSerial items)
      itemFilter =
        fromMaybe fuzzyMonotonic customFilter
      consume event = do
        menuAction <- lowerMaybe (menuConsumerEvent event)
        pure (fromMaybe (eventAction event) (join menuAction))
      quit =
        lower_ do
        sendControlEvent PromptControlEvent.Quit
      in
        pur $
        Stream.finally quit $
        Stream.mapMaybeM (fmap join . lowerMaybe . outputAction) $
        Stream.tapAsync (Fold.drainBy handleAction) $
        Stream.mapM consume $
        Stream.parallelFst prompt menuItems

menuResult ::
  Members [Log, Embed IO] r =>
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
  Members [MenuState i, MenuConsumer a, MenuRenderer i, Reader (MenuConfig i)] r =>
  Members [Log, Time t d, Mask res, Race, Resource, Embed IO, Final IO] r =>
  Sem r (MenuResult a)
menuMain =
  withPromptStream \ promptEvents -> do
    stream <- menuStream (Stream.fromSerial promptEvents)
    menuResult =<< embed (Stream.last stream)

simpleMenu ::
  Show a =>
  Members [Scoped pres PromptRenderer, PromptEvents, PromptInput, PromptState, MenuRenderer i] r =>
  Members [Log, Time t d, Mask mres, Resource, Race, Embed IO, Final IO] r =>
  InterpreterFor (MenuConsumer a) (MenuStack i ++ PromptRenderer : r) ->
  MenuConfig i ->
  [PromptFlag] ->
  Sem r (MenuResult a)
simpleMenu consumer config flags =
  withPrompt do
    interpretMenu config flags $ consumer $ menuMain

menu ::
  ∀ a i pres mrres res t d r .
  Show a =>
  Members (MenuStack i) r =>
  Members [MenuConsumer a, PromptInput, Scoped pres PromptRenderer] r =>
  Members [Scoped mrres (MenuRenderer i), Log, Time t d, Mask res, Race, Resource, Embed IO, Final IO] r =>
  Sem r (MenuResult a)
menu =
  withMenuRenderer $ withPrompt do
    menuMain
