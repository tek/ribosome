module Ribosome.Menu.Interpreter.MenuStream where

import qualified Data.List.NonEmpty as NonEmpty
import Polysemy.Final (bindS, getInitialStateS, getInspectorS, interpretFinal, runS)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Internal.Data.Stream.IsStream (after_)

import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.RenderEvent (RenderEvent (RenderEvent))
import Ribosome.Menu.Effect.MenuStream (MenuStream (MenuStream))
import Ribosome.Menu.Stream.Accumulate (mapMAcc)
import Ribosome.Menu.Stream.Util (repeatUntilNothing)

interpretMenuStream ::
  Member (Final IO) r =>
  InterpreterFor MenuStream r
interpretMenuStream =
  interpretFinal \case
    MenuStream items promptEventsM queryUpdateM insertM renderEventM menuEventM ->
      handle
        where
          handle = do
            s <- getInitialStateS
            Inspector (ins :: ∀ y . f y -> Maybe y) <- getInspectorS
            promptEvents <- runS promptEventsM
            queryUpdate <- bindS queryUpdateM
            insertItems <- bindS insertM
            renderEvent <- bindS renderEventM
            menuEvent <- bindS menuEventM
            let
              pureS :: ∀ b . b -> f b
              pureS =
                (<$ s)
              maybeF :: ∀ b . f (Maybe b) -> Maybe (f b)
              maybeF =
                fmap (<$ s) <=< ins
              prompt =
                mapMAcc (pure . Left) (queryUpdate . NonEmpty.last) $
                repeatUntilNothing (maybeF <$> promptEvents)
              menuItems =
                after_ (menuEvent (pureS MenuEvent.Exhausted)) $
                Stream.mapM insert $
                Stream.foldIterateM chunker (pure []) $
                Stream.fromSerial items
                where
                  insert new =
                    pureS (RenderEvent "new items") <$ insertItems (pureS new)
                  chunker = pure . \case
                    [] ->
                      Fold.take 100 Fold.toList
                    _ ->
                      Fold.take 10000 Fold.toList
            pure $
              fmap pureS $
              Stream.fold (Fold.drainBy renderEvent) $
              Stream.parallelFst prompt menuItems
