module Ribosome.Interpreter.Scratch where

import Conc (interpretAtomic)
import qualified Data.Map.Strict as Map
import Exon (exon)

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.ScratchId (ScratchId (ScratchId))
import Ribosome.Data.ScratchState (ScratchState)
import Ribosome.Effect.Scratch (Scratch (Find, Get, Kill, Show, Update))
import Ribosome.Host.Data.RpcError (RpcError (RpcError))
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Scratch (killScratch, lookupScratch, setScratchContent, showInScratch)

interpretScratchAtomic ::
  Members [Rpc !! RpcError, AtomicState (Map ScratchId ScratchState), Reader PluginName, Log, Resource] r =>
  InterpreterFor (Scratch !! RpcError) r
interpretScratchAtomic =
  interpretResumable \case
    Show text options ->
      restop (showInScratch text options)
    Update i text -> do
      s <- stopNote (RpcError [exon|No scratch buffer named '#{coerce i}' exists|]) =<< lookupScratch i
      s <$ restop @_ @Rpc (setScratchContent s text)
    Kill i ->
      traverse_ killScratch =<< lookupScratch i
    Get ->
      atomicGets Map.elems
    Find i ->
      atomicGets (Map.lookup i)

interpretScratch ::
  Members [Rpc !! RpcError, Reader PluginName, Log, Resource, Embed IO] r =>
  InterpreterFor (Scratch !! RpcError) r
interpretScratch =
  interpretAtomic mempty .
  interpretScratchAtomic .
  raiseUnder
