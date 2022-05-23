module Ribosome.Menu.Stream.ChunkAfter where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Sequence ((|>))
import Prelude hiding (output)
import qualified Streamly.Internal.Data.Fold as Fold
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Fold.Step (Step (Done, Partial))
import Streamly.Internal.Data.Fold.Type (mkFoldM)
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Internal.Data.Time.Units (MilliSecond64, RelTime, toRelTime)
import Streamly.Prelude (IsStream)

data CAState a =
  CAState {
    prevTick :: Int,
    curTick :: Int,
    buffer :: Seq a
  }
  deriving stock (Eq, Show)

data ChunkAfter c e s =
  Chunk c
  |
  Emit e
  |
  Skip s
  deriving stock (Eq, Show)

data Chunked c e s =
  Chunked (NonEmpty c)
  |
  Emitter e
  |
  Skipped s
  deriving stock (Eq, Show)

toChunked :: ChunkAfter c e s -> Chunked c e s
toChunked = \case
  Chunk c -> Chunked [c]
  Emit e -> Emitter e
  Skip s -> Skipped s

relSeconds :: Double -> RelTime
relSeconds secs =
  toRelTime (round (secs * 1000) :: MilliSecond64)

extract :: CAState c -> (CAState c, [Chunked c e s])
extract CAState {..} =
  (CAState {buffer = mempty, prevTick = curTick, ..}, output)
  where
    output =
      maybe [] (pure . Chunked) (nonEmpty (toList buffer))

elemStep :: CAState c -> ChunkAfter c e s -> Step (CAState c) (CAState c, [Chunked c e s])
elemStep s@CAState {..} = \case
    Chunk c ->
      Partial (s {buffer = buffer |> c})
    Emit e ->
      let (ns, r) = extract s
      in Done (ns, r ++ [Emitter e])
    Skip sk ->
      Done (s { prevTick = curTick }, [Skipped sk])

tickStep :: Int -> CAState c -> Int -> Step (CAState c) (CAState c, [Chunked c e s])
tickStep timeout s@CAState {..} tick =
  if curTick - prevTick >= timeout
  then Done (extract s)
  else Partial s {curTick = tick}

chunkAfterFold ::
  Monad m =>
  Int ->
  (a -> ChunkAfter c e s) ->
  CAState c ->
  Fold m (Either Int a) (CAState c, [Chunked c e s])
chunkAfterFold timeout classify initial =
  mkFoldM step (pure (Partial initial)) (pure . extract)
  where
    step s =
      pure . either (tickStep timeout s) (elemStep s . classify)

chunkAfterIteration ::
  Monad m =>
  Int ->
  (a -> ChunkAfter c e s) ->
  Either Int (CAState c, [Chunked c e s]) ->
  m (Fold m (Either Int a) (Either Int (CAState c, [Chunked c e s])))
chunkAfterIteration timeout classify e = do
  pure . either pristine update $ e
  where
    pristine tick =
      Fold.head <&> \case
        Just (Right a) -> Right (CAState tick tick mempty, [toChunked (classify a)])
        _ -> Left 0
    update (s, _) =
      Right . second toList <$> chunkAfterFold timeout classify s

chunkAfter ::
  MonadIO m =>
  IsStream t =>
  MonadThrow m =>
  Functor (t m) =>
  MonadBaseControl IO m =>
  Double ->
  Double ->
  (a -> ChunkAfter c e s) ->
  t m a ->
  t m (Chunked c e s)
chunkAfter clock timeout classify =
  Stream.concatMap (Stream.fromList . snd) .
  Stream.rights .
  Stream.foldIterateM (chunkAfterIteration (round (timeout / clock)) classify) (pure (Left 0)) .
  flip Stream.parallelFst (Left <$> Stream.delay clock (Stream.enumerateFrom 0)) .
  Stream.map Right
