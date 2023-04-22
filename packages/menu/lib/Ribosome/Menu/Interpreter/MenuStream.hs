module Ribosome.Menu.Interpreter.MenuStream where

import qualified Data.List.NonEmpty as NonEmpty
import Polysemy.Final (bindS, getInitialStateS, getInspectorS, interpretFinal, runS)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (SerialT)

import Ribosome.Menu.Effect.MenuStream (MenuStream (MenuStream))
import Ribosome.Menu.Stream.Accumulate (accLeftBusy)
import Ribosome.Menu.Stream.Util (nothingTerminated, repeatUntilNothing)

interpretMenuStream ::
  Member (Final IO) r =>
  InterpreterFor MenuStream r
interpretMenuStream =
  interpretFinal \case
    MenuStream items promptEventsM queryUpdateM insertM renderEventM exhaustedM -> do
      s <- getInitialStateS
      Inspector (ins :: ∀ y . f y -> Maybe y) <- getInspectorS
      promptEvents <- runS promptEventsM
      queryUpdate <- bindS queryUpdateM
      insertItems <- bindS insertM
      renderEvent <- bindS renderEventM
      exhausted <- runS exhaustedM
      let
        pureS :: ∀ b . b -> f b
        pureS = (<$ s)

        maybeF :: ∀ b . f (Maybe b) -> Maybe (f b)
        maybeF = fmap (<$ s) <=< ins

        prompt = repeatUntilNothing @SerialT (maybeF <$> promptEvents)

        insert new = insertItems (pureS new)

        chunker = pure . \case
          [] -> Fold.take 100 Fold.toList
          _ -> Fold.take 10000 Fold.toList

        menuItems =
          Stream.foldIterateM chunker (pure []) $
          Stream.fromSerial items

        handleItems = \case
          Just i -> Just <$> insert i
          Nothing -> Nothing <$ exhausted

        handlePrompts =
          fmap Just . queryUpdate . NonEmpty.last

        stream =
          Stream.fold (Fold.drainBy (traverse_ renderEvent)) $
          accLeftBusy handlePrompts handleItems $
          Stream.parallelFst (Left <$> prompt) (Right <$> nothingTerminated menuItems)

      pure (pureS <$> stream)
