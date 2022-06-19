module Ribosome.Menu.Stream.Accumulate where

import Control.Exception (finally)
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

newtype Work r =
  Work { unWork :: IO (Maybe r) }

work ::
  MVar () ->
  IO (Maybe r) ->
  Work r
work mv mr =
  Work (finally mr (tryPutMVar mv ()))

extract ::
  (NonEmpty c -> IO r) ->
  CWState r c ->
  (CWState r c, Work r)
extract consume s@CWState {..} =
  (s { buffer = mempty }, output (nonEmpty (toList buffer)))
  where
    output = \case
      Just as ->
        work lock (Just <$> consume as)
      Nothing ->
        Work (pure Nothing)

elemStep ::
  (NonEmpty c -> IO r) ->
  CWState r c ->
  Either c r ->
  IO (Step (CWState r c) (CWState r c, Work r))
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
  (a -> IO (Either c r)) ->
  (NonEmpty c -> IO r) ->
  CWState r c ->
  Fold IO a (CWState r c, Work r)
chunkWhileFold classify consume initial =
  mkFoldM step (pure (Partial initial)) (pure . extract consume)
  where
    step s =
      elemStep consume s <=< classify

chunkWhileIteration ::
  (a -> IO (Either c r)) ->
  (NonEmpty c -> IO r) ->
  (CWState r c, Maybe (Work r)) ->
  IO (Fold IO a (CWState r c, Maybe (Work r)))
chunkWhileIteration classify consume (initial, _) =
  pure (second Just <$> chunkWhileFold classify consume initial)

chunkWhileMain ::
  IsStream t =>
  Functor (t IO) =>
  (a -> IO (Either c r)) ->
  (NonEmpty c -> IO r) ->
  CWState r c ->
  t IO a ->
  t IO (Work r)
chunkWhileMain classify consume initial =
  Stream.catMaybes .
  Stream.map snd .
  Stream.foldIterateM (chunkWhileIteration classify consume) (pure (initial, Nothing))

mapMAcc ::
  IsStream t =>
  Functor (t IO) =>
  (a -> IO (Either c r)) ->
  (NonEmpty c -> IO r) ->
  t IO a ->
  t IO r
mapMAcc classify consume str =
  Stream.bracket_ (CWState mempty <$> liftIO (newMVar ())) (const unit) \ initial ->
    Stream.catMaybes $
    Stream.concatMapWith Stream.ahead (Stream.fromEffect . unWork) $
    chunkWhileMain classify consume initial
    str

mapMAccMaybe ::
  IsStream t =>
  Functor (t IO) =>
  (a -> IO (Maybe r)) ->
  IO r ->
  t IO a ->
  t IO r
mapMAccMaybe classify consume =
  mapMAcc (fmap (maybeToRight ()) . classify) (const consume)
