module Ribosome.Plugin.Mapping where

import Data.MessagePack (Object(ObjectString, ObjectNil))

import Ribosome.Data.Mapping (MappingError(InvalidArgs, NoSuchMapping), MappingIdent(MappingIdent))

data MappingHandler m =
  MappingHandler {
    mhMapping :: MappingIdent,
    mhHandler :: m ()
  }

mappingHandler :: Text -> m () -> MappingHandler m
mappingHandler ident =
  MappingHandler (MappingIdent ident)

mapping :: MappingIdent -> [MappingHandler m] -> Maybe (MappingHandler m)
mapping ident =
  find ((ident ==) . mhMapping)

noSuchMapping =
  throwHoist . NoSuchMapping

executeMapping :: MappingHandler m -> m ()
executeMapping (MappingHandler _ f) =
  f

handleMappingRequest mappings [ObjectString s] =
  ObjectNil <$ maybe (noSuchMapping ident) executeMapping (mapping ident mappings)
  where
    ident = MappingIdent (decodeUtf8 s)
handleMappingRequest _ args =
  throwHoist (InvalidArgs args)
