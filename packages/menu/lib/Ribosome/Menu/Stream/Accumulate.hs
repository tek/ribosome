module Ribosome.Menu.Stream.Accumulate where

import Data.Sequence (Seq (Empty, (:<|), (:|>)), (|>))
import Prelude hiding (consume, finally, output)
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream.Prelude as Stream

overMVar :: MVar a -> (a -> (a, b)) -> IO b
overMVar v f =
  modifyMVar v (pure . f)

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

nonEmptyBuffer :: Seq a -> a -> NonEmpty a
nonEmptyBuffer (h :<| t) a =
  h :| (toList (t |> a))
nonEmptyBuffer Empty a =
  pure a

emitBuffer ::
  (NonEmpty a -> IO r) ->
  Seq a ->
  a ->
  Stream IO r
emitBuffer handleAcc buffer a =
  Stream.fromEffect (handleAcc (nonEmptyBuffer buffer a))

bufferedElement ::
  (NonEmpty a -> IO r) ->
  a ->
  BusyEnv r a ->
  (BusyEnv r a, Stream IO r)
bufferedElement handleAcc a = \case
  BusyEnv {buffer, state = Idle} ->
    (BusyEnv {buffer = [], state = Busy 1}, emitBuffer handleAcc buffer a)
  BusyEnv {buffer, state = Busy n} ->
    (BusyEnv {buffer = buffer |> a, state = Busy n}, Stream.nil)

element ::
  (NonEmpty a -> IO r) ->
  (b -> IO r) ->
  MVar (BusyEnv r a) ->
  Either a b ->
  Stream IO (Stream IO r)
element handleAcc handleReg envVar el =
  Stream.fromEffect $ overMVar envVar \ env ->
    case el of
      Left a ->
        (bufferedElement handleAcc a env)
      Right b ->
        (increment env, Stream.fromEffect (handleReg b))

checkBuffer ::
  (NonEmpty a -> IO r) ->
  MVar (BusyEnv r a) ->
  Stream IO r
checkBuffer handleAcc envVar =
  Stream.concatEffect $ overMVar envVar \case
    env | (buf :|> a) <- env.buffer ->
      (env {buffer = []}, Stream.append (emitBuffer handleAcc buf a) (checkBuffer handleAcc envVar))
        | otherwise ->
      (decrement env, Stream.nil)

accLeftBusy ::
  (NonEmpty a -> IO r) ->
  (b -> IO r) ->
  Stream IO (Either a b) ->
  Stream IO r
accLeftBusy handleAcc handleReg str =
  Stream.concatMap use (Stream.fromEffect (liftIO (newMVar (BusyEnv [] Idle))))
  where
    use envVar =
      Stream.concatMap (\ r -> Stream.append r (checkBuffer handleAcc envVar)) $
      Stream.parConcatMap (Stream.ordered True) (element handleAcc handleReg envVar) $
      str
