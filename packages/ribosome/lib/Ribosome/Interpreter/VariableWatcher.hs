module Ribosome.Interpreter.VariableWatcher where

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

interpretVariableWatcher ::
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpreterFor (VariableWatcher !! HandlerError) r
interpretVariableWatcher vars =
  interpretResumable \case
    VariableWatcher.WatchedVariables ->
      pure (Map.keys vars)
    VariableWatcher.Update new var -> do
      handler <- stopNote (HandlerError.simple [exon|No handler for variable #{coerce var}|]) (Map.lookup var vars)
      handler new
