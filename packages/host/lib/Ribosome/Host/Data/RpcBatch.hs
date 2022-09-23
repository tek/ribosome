module Ribosome.Host.Data.RpcBatch where

import Data.MessagePack (Object)

import Ribosome.Host.Class.Msgpack.Error (DecodeError)
import Ribosome.Host.Data.Request (Request)

data RpcBatch :: Type -> Type where
  Pure :: a -> RpcBatch a
  Bind :: RpcBatch a -> (a -> RpcBatch b) -> RpcBatch b
  Request :: Request -> (Object -> Either DecodeError a) -> RpcBatch a

instance Functor RpcBatch where
  fmap f = \case
    Pure a ->
      Pure (f a)
    Bind fa g ->
      Bind fa (fmap f . g)
    Request req g ->
      Request req (fmap f . g)
