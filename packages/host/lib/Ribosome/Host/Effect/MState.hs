-- |A state effect that allows atomic updates with monadic actions.
module Ribosome.Host.Effect.MState where

-- |A state effect that allows atomic updates with monadic actions.
--
-- The constructor 'muse' is analogous to the usual @state@ combinator, in that it transforms the state monadically
-- alongside a return value, but unlike 'State' and 'AtomicState', the callback may be a 'Sem'.
--
-- This is accomplished by locking every call with an 'MVar'.
--
-- For read-only access to the state that doesn't care about currently running updates, the constructor 'mread' directly
-- returns the state without consulting the lock.
data MState s :: Effect where
  -- |Run a monadic action on the state in a mutually exclusive fashion that additionally returns a value.
  Use :: (s -> m (s, a)) -> MState s m a
  -- |Obtain the current state.
  Read :: MState s m s

-- |Run a monadic action on the state in a mutually exclusive fashion that additionally returns a value.
muse ::
  Member (MState s) r =>
  (s -> Sem r (s, a)) ->
  Sem r a
muse =
  send . Use

-- |Run a monadic action on the state in a mutually exclusive fashion.
mtrans ::
  Member (MState s) r =>
  (s -> Sem r s) ->
  Sem r ()
mtrans f =
  muse (fmap (,()) . f)

-- |Apply a pure function to the state that additionally returns a value.
mstate ::
  Member (MState s) r =>
  (s -> (s, a)) ->
  Sem r a
mstate f =
  muse (pure . f)

-- |Apply a pure function to the state.
mmodify ::
  Member (MState s) r =>
  (s -> s) ->
  Sem r ()
mmodify f =
  mtrans (pure . f)

-- |Replace the state.
mput ::
  Member (MState s) r =>
  s ->
  Sem r ()
mput s =
  mmodify (const s)

-- |Obtain the current state.
mread ::
  Member (MState s) r =>
  Sem r s
mread =
  send Read

-- |Obtain the current state, transformed by a pure function.
mreads ::
  Member (MState s) r =>
  (s -> a) ->
  Sem r a
mreads f =
  f <$> mread

-- |Interpret 'State' in terms of 'MState'.
stateToMState ::
  Member (MState s) r =>
  InterpreterFor (State s) r
stateToMState sem =
  muse \ s ->
    runState s sem

-- |A 'Scoped' alias for 'MState' that allows running it on a local region without having to involve `IO` in the stack.
type ScopedMState s =
  Scoped s () (MState s)

-- |Run a 'Scoped' 'MState' on a local region without having to involve `IO` in the stack.
withMState ::
  Member (ScopedMState s) r =>
  s ->
  InterpreterFor (MState s) r
withMState =
  scoped
