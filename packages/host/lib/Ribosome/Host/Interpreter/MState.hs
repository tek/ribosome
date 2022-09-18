-- |Interpreters for 'MState'.
module Ribosome.Host.Interpreter.MState where

import Conc (Lock, interpretAtomic, interpretLockReentrant, interpretScopedWithH, lock)
import Polysemy.Internal.Tactics (liftT)

import qualified Ribosome.Host.Effect.MState as MState
import Ribosome.Host.Effect.MState (MState, ScopedMState)

-- |Interpret 'MState' using 'AtomicState' and 'Lock'.
interpretMState ::
  Members [Resource, Race, Mask mres, Embed IO] r =>
  s ->
  InterpreterFor (MState s) r
interpretMState initial =
  interpretLockReentrant .
  interpretAtomic initial .
  reinterpret2H \case
    MState.Use f ->
      lock do
        s0 <- atomicGet
        res <- runTSimple (f s0)
        Inspector ins <- getInspectorT
        for_ (ins res) \ (s, _) -> atomicPut s
        pure (snd <$> res)
    MState.Read ->
      liftT atomicGet

-- |Interpret 'MState' as 'State'.
evalMState ::
  s ->
  InterpreterFor (MState s) r
evalMState initial =
  evalState initial .
  reinterpretH \case
    MState.Use f -> do
      s0 <- raise get
      res <- runTSimple (f s0)
      Inspector ins <- getInspectorT
      for_ (ins res) \ (s, _) -> put s
      pure (snd <$> res)
    MState.Read ->
      liftT get

-- |Internal combinator that runs the dependencies of the scope for 'MState'.
scope ::
  Members [Mask mres, Resource, Race, Embed IO] r =>
  s ->
  (() ->
  Sem (AtomicState s : Lock : r) a) ->
  Sem r a
scope initial use =
  interpretLockReentrant $ interpretAtomic initial $ use ()

-- |Interpret 'MState' as a scoped effect.
interpretMStates ::
  ∀ s mres r .
  Members [Mask mres, Resource, Race, Embed IO] r =>
  InterpreterFor (ScopedMState s) r
interpretMStates =
  interpretScopedWithH @[AtomicState s, Lock] scope \ () -> \case
    MState.Use f ->
      lock do
        s0 <- atomicGet
        res <- runTSimple (f s0)
        Inspector ins <- getInspectorT
        for_ (ins res) \ (s, _) -> atomicPut s
        pure (snd <$> res)
    MState.Read ->
      liftT atomicGet
