-- |Abstraction of 'RpcCall' and 'Sem'
module Ribosome.Host.Class.MonadRpc where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))
import Ribosome.Host.Data.Request (Request)
import Ribosome.Host.Data.RpcCall (RpcCall (RpcRequest))
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)

type MonadRpc :: (Type -> Type) -> Constraint

-- |This class abstracts over the data representation of RPC calls, 'RpcCall', and their reified form in 'Sem'.
--
-- This allows composite functions that send multiple requests to the Neovim API to use the atomic call batching feature
-- without having to specialize to 'RpcCall', which would require the use of @Rpc.sync@ to convert them to 'Sem'.
--
-- Note that a function written with 'MonadRpc' still needs to call 'atomic' on regions that should use atomic calls,
-- otherwise a callsite instantiating @m@ to 'Sem' will not take advantage of them.
--
-- The easiest way to do this is to wrap the function body with 'atomic':
--
-- > aAndB :: MonadRpc m => m (Int, Int)
-- > aAndB =
-- >   atomic do
-- >     a <- nvimGetVar "a"
-- >     b <- nvimGetVar "b"
-- >     pure (a, b)
--
-- This works because @nvimGetVar@ is polymorphic in the monad, so it is instantiated as 'RpcCall', after which the
-- call to 'atomic' converts it back to the polymorphic type.
class (
    Monad m,
    (∀ a . Monoid a => Monoid (m a))
  ) => MonadRpc m where
    -- |Instantiate a program to 'RpcCall' so it is guaranteed to use @nvim_call_atomic@ when possible.
    atomic :: RpcCall a -> m a

instance MonadRpc RpcCall where
  atomic =
    id
  {-# inline atomic #-}

instance (
    Member Rpc r
  ) => MonadRpc (Sem r) where
    atomic =
      Rpc.sync
    {-# inline atomic #-}

-- |Lift a 'Request' into @m@.
rpcRequest ::
  MonadRpc m =>
  MsgpackDecode a =>
  Request ->
  m a
rpcRequest req =
  atomic (RpcRequest req fromMsgpack)
{-# inline rpcRequest #-}
