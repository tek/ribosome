module Ribosome.Plugin.Mapping where

import Control.Monad.DeepError (MonadDeepError(throwHoist))
import Data.ByteString.UTF8 (toString)
import Data.Foldable (find)
import Data.MessagePack (Object(ObjectString, ObjectNil))

import Ribosome.Data.Mapping (MappingError(InvalidArgs, NoSuchMapping), MappingIdent(MappingIdent))

data MappingHandler m =
  MappingHandler {
    mhMapping :: MappingIdent,
    mhHandler :: m Object
  }

mappingHandler :: Functor m => String -> m () -> MappingHandler m
mappingHandler ident m =
  MappingHandler (MappingIdent ident) (ObjectNil <$ m)

mapping :: MappingIdent -> [MappingHandler m] -> Maybe (MappingHandler m)
mapping ident =
  find ((ident ==) . mhMapping)

noSuchMapping :: MonadDeepError e MappingError m => MappingIdent -> m a
noSuchMapping =
  throwHoist . NoSuchMapping

executeMapping :: MappingHandler m -> m Object
executeMapping (MappingHandler _ f) =
  f

handleMappingRequest :: MonadDeepError e MappingError m => [MappingHandler m] -> [Object] -> m Object
handleMappingRequest mappings [ObjectString s] =
  maybe (noSuchMapping ident) executeMapping (mapping ident mappings)
  where
    ident = MappingIdent (toString s)
handleMappingRequest _ args =
  throwHoist (InvalidArgs args)
