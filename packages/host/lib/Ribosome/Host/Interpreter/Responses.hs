module Ribosome.Host.Interpreter.Responses where

import qualified Data.Map.Strict as Map
import Exon (exon)
import Polysemy.Conc (interpretAtomic)

import Ribosome.Host.Data.RpcError (RpcError (RpcError))
import Ribosome.Host.Effect.Responses (Responses (Add, Respond, Wait))
import Ribosome.Host.Interpreter.Id (interpretInputNum)

failAbsentKey ::
  Show k =>
  Member (Stop RpcError) r =>
  k ->
  (a -> Sem r b) ->
  Maybe a ->
  Sem r b
failAbsentKey k f = \case
  Just resp ->
    f resp
  Nothing ->
    stop (RpcError [exon|No response registered for #{show k}|])

interpretResponsesAtomic ::
  ∀ k v r .
  Ord k =>
  Show k =>
  Members [Input k, AtomicState (Map k (MVar v)), Embed IO] r =>
  InterpreterFor (Responses k v !! RpcError) r
interpretResponsesAtomic =
  interpretResumable \case
    Add -> do
      k <- input
      resp <- embed newEmptyMVar
      k <$ atomicModify' (Map.insert k resp)
    Wait k ->
      failAbsentKey k (embed . takeMVar) =<< atomicGets (Map.lookup k)
    Respond k v -> do
      stored <- atomicState' (swap . Map.updateLookupWithKey (\ _ _ -> Nothing) k)
      failAbsentKey k (void . embed . flip tryPutMVar v) stored

interpretResponses ::
  ∀ k v r .
  Ord k =>
  Num k =>
  Show k =>
  Member (Embed IO) r =>
  InterpreterFor (Responses k v !! RpcError) r
interpretResponses =
  interpretAtomic (mempty :: Map k (MVar v)) .
  interpretInputNum .
  interpretResponsesAtomic .
  raiseUnder2
