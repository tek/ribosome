module Ribosome.Host.Effect.MState where
import Conc (PScoped, pscoped)

data MState s :: Effect where
  Use :: (s -> m (s, a)) -> MState s m a
  Read :: MState s m s

muse ::
  Member (MState s) r =>
  (s -> Sem r (s, a)) ->
  Sem r a
muse =
  send . Use

mtrans ::
  Member (MState s) r =>
  (s -> Sem r s) ->
  Sem r ()
mtrans f =
  muse (fmap (,()) . f)

mstate ::
  Member (MState s) r =>
  (s -> (s, a)) ->
  Sem r a
mstate f =
  muse (pure . f)

mmodify ::
  Member (MState s) r =>
  (s -> s) ->
  Sem r ()
mmodify f =
  mtrans (pure . f)

mread ::
  Member (MState s) r =>
  Sem r s
mread =
  send Read

mreads ::
  Member (MState s) r =>
  (s ->  a) ->
  Sem r a
mreads f =
  f <$> mread

stateToMState ::
  Member (MState s) r =>
  InterpreterFor (State s) r
stateToMState sem =
  muse \ s ->
    runState s sem

type ScopedMState s =
  PScoped s () (MState s)

withMState ::
  Member (ScopedMState s) r =>
  s ->
  InterpreterFor (MState s) r
withMState =
  pscoped
