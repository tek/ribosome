module Ribosome.Host.Interpreter.MState where

import Conc (interpretAtomic, interpretLockReentrant, lock)
import Polysemy.Internal.Tactics (liftT)

import qualified Ribosome.Host.Effect.MState as MState
import Ribosome.Host.Effect.MState (MState)

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
