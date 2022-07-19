module Ribosome.Host.Data.RpcCall where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Data.Request (Request)

type RpcCall :: Type -> Type

-- |A wrapper for 'Request' that allows applicative sequencing of calls for batch processing, used for a declarative
-- representation of the Neovim API.
--
-- Neovim has an API function named @nvim_call_atomic@ that makes it possible to send multiple RPC requests at once,
-- reducing the communcation overhead.
-- Applicative sequences of 'RpcCall's are automatically batched into a single call by 'Ribosome.Rpc'.
--
-- This can be combined neatly with @ApplicativeDo@:
--
-- > import Ribosome
-- > import qualified Ribosome.Api.Data as Api
-- >
-- > sync do
-- >   a :: Int <- Api.nvimGetVar "number1"
-- >   b :: Int <- Api.nvimGetVar "number2"
-- >   pure (a + b)
data RpcCall a where
  RpcCallRequest :: MsgpackDecode a => Request -> RpcCall a
  RpcPure :: a -> RpcCall a
  RpcFmap :: (a -> b) -> RpcCall a -> RpcCall b
  RpcAtomic :: (a -> b -> c) -> RpcCall a -> RpcCall b -> RpcCall c

instance Functor RpcCall where
  fmap f = \case
    RpcCallRequest req ->
      RpcFmap f (RpcCallRequest req)
    RpcPure a ->
      RpcPure (f a)
    RpcFmap g a ->
      RpcFmap (f . g) a
    RpcAtomic g a b ->
      RpcAtomic (\ x y -> f (g x y)) a b

instance Applicative RpcCall where
  pure =
    RpcPure
  liftA2 =
    RpcAtomic

instance (
    Semigroup a
  ) => Semigroup (RpcCall a) where
    (<>) =
      liftA2 (<>)

instance (
    Monoid a
  ) => Monoid (RpcCall a) where
    mempty =
      pure mempty
