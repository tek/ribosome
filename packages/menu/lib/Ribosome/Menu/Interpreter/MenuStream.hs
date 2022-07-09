module Ribosome.Menu.Interpreter.MenuStream where

import Polysemy.Final (bindS, getInitialStateS, getInspectorS, interpretFinal, pureS, runS)
import Prelude hiding (consume)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream

import Ribosome.Host.Interpret (with)
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Effect.MenuStream (MenuStream (MenuStream))
import Ribosome.Menu.Stream.Accumulate (mapMAccMaybe)

interpretMenuStream ::
  ∀ i r .
  Members [Reader (MenuConfig i), Final IO] r =>
  InterpreterFor (MenuStream i) r
interpretMenuStream =
  with ask \ (MenuConfig items) ->
    interpretFinal \case
      MenuStream promptEvents handleActionM classifyM queryUpdateM insertM quitM consumeM outputActionM ->
        handle
          where
            handle = do
              s <- getInitialStateS
              Inspector (ins :: ∀ y . f y -> Maybe y) <- getInspectorS
              handleAction <- bindS handleActionM
              classify <- bindS (uncurry classifyM)
              queryUpdate <- runS queryUpdateM
              insertItems <- bindS insertM
              quit <- runS quitM
              consume <- bindS consumeM
              outputAction <- bindS outputActionM
              let
                fOr fb m =
                  fromMaybe fb (ins m)
                maybeF :: ∀ t b . Functor t => t (f (Maybe b)) -> t (Maybe (f b))
                maybeF =
                  fmap (fmap (<$ s) <=< ins)
                cls pp =
                  maybeF (classify pp)
                prompt =
                  Stream.fromAsync $
                  mapMAccMaybe cls queryUpdate $
                  Stream.mkAsync ((<$ s) <$> promptEvents)
                menuItems =
                  flip Stream.serial (Stream.fromPure (MenuEvent.Exhausted <$ s)) $
                  Stream.mapM insert $
                  Stream.foldIterateM chunker (pure []) $
                  Stream.fromSerial items
                  where
                    insert new =
                      (MenuEvent.NewItems <$ s) <$ insertItems (new <$ s)
                    chunker = pure . \case
                      [] ->
                        Fold.take 100 Fold.toList
                      _ ->
                        Fold.take 10000 Fold.toList
              pureS $
                fmap (fOr MenuResult.Aborted) $
                Stream.finally quit $
                Stream.mapMaybeM (maybeF . outputAction) $
                Stream.tapAsync (Fold.drainBy handleAction) $
                Stream.mapM consume $
                Stream.parallelFst prompt menuItems
