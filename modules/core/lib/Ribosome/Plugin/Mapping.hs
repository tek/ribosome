module Ribosome.Plugin.Mapping where

import Control.Monad.DeepError (MonadDeepError(throwHoist))
import Data.Foldable (find)
import Data.MessagePack (Object(ObjectString, ObjectNil))

import Ribosome.Data.Mapping (MappingError(InvalidArgs, NoSuchMapping), MappingIdent(MappingIdent))

data MappingHandler m =
  MappingHandler {
    mhMapping :: MappingIdent,
    mhHandler :: m ()
  }

mappingHandler :: Functor m => Text -> m () -> MappingHandler m
mappingHandler ident =
  MappingHandler (MappingIdent ident)

mapping :: MappingIdent -> [MappingHandler m] -> Maybe (MappingHandler m)
mapping ident =
  find ((ident ==) . mhMapping)

noSuchMapping :: MonadDeepError e MappingError m => MappingIdent -> m a
noSuchMapping =
  throwHoist . NoSuchMapping

executeMapping :: Functor m => MappingHandler m -> m ()
executeMapping (MappingHandler _ f) =
  f

handleMappingRequest :: MonadDeepError e MappingError m => [MappingHandler m] -> [Object] -> m Object
handleMappingRequest mappings [ObjectString s] =
  ObjectNil <$ maybe (noSuchMapping ident) executeMapping (mapping ident mappings)
  where
    ident = MappingIdent (decodeUtf8 s)
handleMappingRequest _ args =
  throwHoist (InvalidArgs args)
