module Ribosome.Menu.Interpreter.PromptStream where

import Control.Concurrent (threadDelay)
import Polysemy.Final (bindS, getInitialStateS, getInspectorS, interpretFinal, pureS, runS)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (SerialT)

import Ribosome.Menu.Effect.PromptStream (PromptStream (PromptStream))
import qualified Ribosome.Menu.Prompt.Data.InputEvent as InputEvent
import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent
import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)
import Ribosome.Menu.Stream.Util (takeUntilNothing)

-- TODO see if the sleeping can be removed
inputEvent ::
  IO (Maybe InputEvent) ->
  SerialT IO PromptInputEvent
inputEvent inEvent =
  spin
  where
    spin =
      Stream.fromEffect inEvent >>= \case
        Just event -> case event of
          InputEvent.Character a ->
            Stream.cons (PromptInputEvent.Character a) spin
          InputEvent.Interrupt ->
            Stream.fromPure PromptInputEvent.Interrupt
          InputEvent.Error e ->
            Stream.fromPure (PromptInputEvent.Error e)
          InputEvent.NoInput ->
            if True then Stream.before (threadDelay 33000) spin else spin
          InputEvent.Unexpected _ ->
            spin
        Nothing ->
          Stream.nil

interpretPromptStream ::
  Member (Final IO) r =>
  InterpreterFor PromptStream r
interpretPromptStream =
  interpretFinal \case
    PromptStream controlEventM renderM promptEventM startM quitM inEventM ->
      handle
        where
          handle = do
            s <- getInitialStateS
            Inspector (ins :: ∀ y . f y -> Maybe y) <- getInspectorS
            controlEvent <- runS controlEventM
            render <- bindS renderM
            promptEvent <- bindS promptEventM
            start <- runS startM
            quit <- runS quitM
            inEvent <- runS inEventM
            let
              maybeF :: ∀ t b . Functor t => t (f (Maybe b)) -> t (Maybe (f b))
              maybeF =
                fmap (fmap (<$ s) <=< ins)
              control =
                takeUntilNothing (Stream.repeatM (maybeF controlEvent))
              withControl =
                Stream.mapMaybe ins $
                Stream.tapAsync (Fold.drainBy (render . fmap fst)) $
                takeUntilNothing $
                Stream.mapM (maybeF . promptEvent) $
                Stream.parallelMin (fmap Left <$> Stream.before start control) ((<$ s) . Right <$> sourceWithInit)
                where
                  sourceWithInit =
                    Stream.cons PromptInputEvent.Init (inputEvent (ins <$> inEvent))
            pureS (Stream.finally quit withControl)
