module Ribosome.Menu.Stream.Accumulate where

import qualified Control.Concurrent.Lifted as Lifted
import Control.Exception.Lifted (finally)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Sequence ((|>))
import Prelude hiding (consume, finally, output)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Fold.Step (Step (Done, Partial))
import Streamly.Internal.Data.Fold.Type (mkFoldM)
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (IsStream)

data CWState r c =
  CWState {
    buffer :: Seq c,
    lock :: MVar ()
  }

newtype Work m r =
  Work { unWork :: m (Maybe r) }

work ::
  MonadBaseControl IO m =>
  MVar () ->
  m (Maybe r) ->
  Work m r
work mv mr =
  Work (finally mr (Lifted.tryPutMVar mv ()))

extract ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (NonEmpty c -> m r) ->
  CWState r c ->
  (CWState r c, Work m r)
extract consume s@CWState {..} =
  (s { buffer = mempty }, output (nonEmpty (toList buffer)))
  where
    output = \case
      Just as ->
        work lock (Just <$> consume as)
      Nothing ->
        Work (pure Nothing)

elemStep ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (NonEmpty c -> m r) ->
  CWState r c ->
  Either c r ->
  m (Step (CWState r c) (CWState r c, Work m r))
elemStep consume s@CWState {..} = \case
    Left c ->
      accumulate s {buffer = buffer |> c} <$> liftIO (tryTakeMVar lock)
    Right sk ->
      pure (Done (s, Work (pure (Just sk))))
  where
    accumulate newS = \case
      Just () ->
        Done (extract consume newS)
      Nothing ->
        Partial newS

chunkWhileFold ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (a -> m (Either c r)) ->
  (NonEmpty c -> m r) ->
  CWState r c ->
  Fold m a (CWState r c, Work m r)
chunkWhileFold classify consume initial =
  mkFoldM step (pure (Partial initial)) (pure . extract consume)
  where
    step s =
      elemStep consume s <=< classify

chunkWhileIteration ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (a -> m (Either c r)) ->
  (NonEmpty c -> m r) ->
  (CWState r c, Maybe (Work m r)) ->
  m (Fold m a (CWState r c, Maybe (Work m r)))
chunkWhileIteration classify consume (initial, _) =
  pure (second Just <$> chunkWhileFold classify consume initial)

chunkWhileMain ::
  MonadIO m =>
  IsStream t =>
  Functor (t m) =>
  MonadBaseControl IO m =>
  (a -> m (Either c r)) ->
  (NonEmpty c -> m r) ->
  CWState r c ->
  t m a ->
  t m (Work m r)
chunkWhileMain classify consume initial =
  Stream.catMaybes .
  Stream.map snd .
  Stream.foldIterateM (chunkWhileIteration classify consume) (pure (initial, Nothing))

mapMAcc ::
  MonadIO m =>
  IsStream t =>
  MonadCatch m =>
  Functor (t m) =>
  MonadBaseControl IO m =>
  (a -> m (Either c r)) ->
  (NonEmpty c -> m r) ->
  t m a ->
  t m r
mapMAcc classify consume str =
  Stream.bracket_ (CWState mempty <$> liftIO (newMVar ())) (const unit) \ initial ->
    Stream.catMaybes $
    Stream.concatMapWith Stream.async (Stream.fromEffect . unWork) $
    chunkWhileMain classify consume initial
    str

mapMAccMaybe ::
  MonadIO m =>
  IsStream t =>
  MonadCatch m =>
  Functor (t m) =>
  MonadBaseControl IO m =>
  (a -> m (Maybe r)) ->
  m r ->
  t m a ->
  t m r
mapMAccMaybe classify consume =
  mapMAcc (fmap (maybeToRight ()) . classify) (const consume)
