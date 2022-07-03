module Ribosome.Interpreter.VariableWatcher where

import Conc (interpretAtomic)
import qualified Data.Map.Strict as Map
import Data.MessagePack (Object)
import Exon (exon)

import Ribosome.Data.WatchedVariable (WatchedVariable (WatchedVariable))
import qualified Ribosome.Effect.VariableWatcher as VariableWatcher
import Ribosome.Effect.VariableWatcher (VariableWatcher)
import qualified Ribosome.Host.Data.HandlerError as HandlerError
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcHandler (Handler)

interpretVariableWatcherNull :: InterpreterFor (VariableWatcher !! HandlerError) r
interpretVariableWatcherNull =
  interpretResumable \case
    VariableWatcher.WatchedVariables -> mempty
    VariableWatcher.Update _ _ -> unit
    VariableWatcher.Unwatch _ -> unit

noHandler :: WatchedVariable -> HandlerError
noHandler var =
  HandlerError.simple [exon|No handler for variable #{coerce var}|]

interpretVariableWatcher ::
  Member (Embed IO) r =>
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpreterFor (VariableWatcher !! HandlerError) r
interpretVariableWatcher vars =
  interpretAtomic vars .
  interpretResumable \case
    VariableWatcher.WatchedVariables ->
      atomicGets Map.keys
    VariableWatcher.Update new var -> do
      handler <- stopNote (noHandler var) =<< atomicGets (Map.lookup var)
      raiseUnder (handler new)
    VariableWatcher.Unwatch var ->
      atomicModify' (Map.delete var)
  . raiseUnder
