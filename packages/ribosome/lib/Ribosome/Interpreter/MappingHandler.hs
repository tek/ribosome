module Ribosome.Interpreter.MappingHandler where

import qualified Data.Map.Strict as Map
import Exon (exon)

import Ribosome.Data.Mapping (MappingIdent (MappingIdent))
import qualified Ribosome.Effect.MappingHandler as MappingHandler
import Ribosome.Effect.MappingHandler (MappingHandler)
import qualified Ribosome.Host.Data.HandlerError as HandlerError
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcHandler (Handler)

noHandler ::
  MappingIdent ->
  HandlerError
noHandler i =
  HandlerError.simple [exon|No mapping handler for #{coerce i}|]

interpretMappingHandlerNull :: InterpreterFor (MappingHandler !! HandlerError) r
interpretMappingHandlerNull =
  interpretResumable \case
    MappingHandler.Call i ->
      stop (noHandler i)

interpretMappingHandler ::
  Map MappingIdent (Handler r ()) ->
  InterpreterFor (MappingHandler !! HandlerError) r
interpretMappingHandler maps =
  interpretResumable \case
    MappingHandler.Call i ->
      join (stopNote (noHandler i) (Map.lookup i maps))
