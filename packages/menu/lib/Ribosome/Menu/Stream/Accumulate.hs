module Ribosome.Menu.Stream.Accumulate where

import Data.Sequence (Seq (Empty, (:<|), (:|>)), (|>))
import Prelude hiding (consume, finally, output)
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (IsStream, SerialT)

data BusyState =
  Idle
  |
  Busy Int
  deriving stock (Eq, Show, Generic)

data BusyEnv r a =
  BusyEnv {
    buffer :: Seq a,
    state :: BusyState
  }

nonEmptyBuffer :: Seq a -> a -> NonEmpty a
nonEmptyBuffer (h :<| t) a =
  h :| (toList (t |> a))
nonEmptyBuffer Empty a =
  pure a

emitBuffer ::
  (NonEmpty a -> IO r) ->
  Seq a ->
  a ->
  SerialT IO r
emitBuffer handleAcc buffer a =
  Stream.fromEffect (handleAcc (nonEmptyBuffer buffer a))

increment :: BusyEnv r a -> BusyEnv r a
increment env =
  env {state = incState env.state}
  where
    incState = \case
      Idle -> Busy 1
      Busy n -> Busy (n + 1)

decrement :: BusyEnv r a -> BusyEnv r a
decrement BusyEnv {..} =
  BusyEnv {state = incState state, ..}
  where
    incState = \case
      Idle -> Idle
      Busy 1 -> Idle
      Busy n -> Busy (n - 1)

bufferedElement ::
  (NonEmpty a -> IO r) ->
  a ->
  BusyEnv r a ->
  (BusyEnv r a, SerialT IO r)
bufferedElement handleAcc a = \case
  BusyEnv {buffer, state = Idle} ->
    (BusyEnv {buffer = [], state = Busy 1}, emitBuffer handleAcc buffer a)
  BusyEnv {buffer, state = Busy n} ->
    (BusyEnv {buffer = buffer |> a, state = Busy n}, Stream.nil)

elemStep ::
  (NonEmpty a -> IO r) ->
  (b -> IO r) ->
  MVar (BusyEnv r a) ->
  Either a b ->
  SerialT IO r
elemStep handleAcc handleReg envVar el =
  Stream.concatM $ modifyMVar envVar \ env ->
    case el of
      Left a ->
        pure (bufferedElement handleAcc a env)
      Right b ->
        pure (increment env, Stream.fromEffect (handleReg b))

checkBuffer ::
  (NonEmpty a -> IO r) ->
  MVar (BusyEnv r a) ->
  SerialT IO r
checkBuffer handleAcc envVar =
  Stream.concatM $ modifyMVar envVar $ pure . \case
    env | (buf :|> a) <- env.buffer ->
      (env {buffer = []}, Stream.serial (emitBuffer handleAcc buf a) (checkBuffer handleAcc envVar))
        | otherwise ->
          (decrement env, Stream.nil)

accLeftBusy ::
  IsStream t =>
  (NonEmpty a -> IO r) ->
  (b -> IO r) ->
  t IO (Either a b) ->
  SerialT IO r
accLeftBusy handleAcc handleReg str =
  Stream.bracket_ (liftIO (newMVar (BusyEnv [] Idle))) (const unit) \ envVar ->
    Stream.concatMap (\ r -> Stream.serial r (checkBuffer handleAcc envVar)) $
    Stream.map (elemStep handleAcc handleReg envVar) $
    Stream.adapt str
