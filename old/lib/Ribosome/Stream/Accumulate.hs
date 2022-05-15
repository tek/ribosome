module Ribosome.Stream.Accumulate where

import Control.Exception.Lifted (finally)
import Control.Monad.Catch (MonadCatch)
import Data.Sequence ((|>))
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
  MonadIO m =>
  MVar () ->
  m (Maybe r) ->
  Work m r
work mv mr =
  Work (finally mr (tryPutMVar mv ()))

extract ::
  MonadIO m =>
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
  (NonEmpty c -> m r) ->
  CWState r c ->
  Either c r ->
  m (Step (CWState r c) (CWState r c, Work m r))
elemStep consume s@CWState {..} = \case
    Left c ->
      accumulate s {buffer = buffer |> c} <$> tryTakeMVar lock
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
  (a -> m (Either c r)) ->
  (NonEmpty c -> m r) ->
  CWState r c ->
  Fold m a (CWState r c, Work m r)
chunkWhileFold classify consume initial =
  mkFoldM step (pure (Partial initial)) (pure . extract consume)
  where
    step s =
      (elemStep consume s <=< classify)

chunkWhileIteration ::
  MonadIO m =>
  (a -> m (Either c r)) ->
  (NonEmpty c -> m r) ->
  (CWState r c, Maybe (Work m r)) ->
  m (Fold m a (CWState r c, Maybe (Work m r)))
chunkWhileIteration classify consume (initial, _) =
  pure (second Just <$> chunkWhileFold classify consume initial)

chunkWhileMain ::
  MonadIO m =>
  IsStream stream =>
  (a -> m (Either c r)) ->
  (NonEmpty c -> m r) ->
  CWState r c ->
  stream m a ->
  stream m (Work m r)
chunkWhileMain classify consume initial =
  Stream.catMaybes .
  Stream.map snd .
  Stream.foldIterateM (chunkWhileIteration classify consume) (pure (initial, Nothing))

mapMAcc ::
  MonadIO m =>
  MonadCatch m =>
  IsStream stream =>
  (a -> m (Either c r)) ->
  (NonEmpty c -> m r) ->
  stream m a ->
  stream m r
mapMAcc classify consume str =
  Stream.bracket_ (CWState mempty <$> newMVar ()) (const unit) \ initial ->
    Stream.catMaybes $
    Stream.concatMapWith Stream.async (Stream.fromEffect . unWork) $
    chunkWhileMain classify consume initial $
    str
