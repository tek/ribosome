-- |Interpreters for 'Scratch'
module Ribosome.Interpreter.Scratch where

import Conc (interpretAtomic)
import qualified Data.Map.Strict as Map
import Exon (exon)

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.ScratchId (ScratchId (ScratchId))
import Ribosome.Data.ScratchState (ScratchState)
import Ribosome.Effect.Scratch (Scratch (Delete, Find, Get, Show, Update))
import qualified Ribosome.Host.Data.RpcError as RpcError
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Internal.Scratch (killScratch, lookupScratch, setScratchContent, showInScratch)

-- |Interpret 'Scratch' by storing the Neovim UI handles in 'AtomicState'.
-- This uses 'Resumable', see [Errors]("Ribosome#g:errors").
interpretScratchAtomic ::
  Members [Rpc !! RpcError, AtomicState (Map ScratchId ScratchState), Reader PluginName, Log, Resource] r =>
  InterpreterFor (Scratch !! RpcError) r
interpretScratchAtomic =
  interpretResumable \case
    Show text options ->
      restop (showInScratch text options)
    Update i text -> do
      s <- stopNote (RpcError.Unexpected [exon|No scratch buffer named '#{coerce i}' exists|]) =<< lookupScratch i
      s <$ restop @_ @Rpc (setScratchContent s text)
    Delete i ->
      traverse_ killScratch =<< lookupScratch i
    Get ->
      atomicGets Map.elems
    Find i ->
      atomicGets (Map.lookup i)

-- |Interpret 'Scratch' by storing the Neovim UI handles in 'AtomicState'.
-- This uses 'Resumable', see [Errors]("Ribosome#g:errors").
interpretScratch ::
  Members [Rpc !! RpcError, Reader PluginName, Log, Resource, Embed IO] r =>
  InterpreterFor (Scratch !! RpcError) r
interpretScratch =
  interpretAtomic mempty .
  interpretScratchAtomic .
  raiseUnder
