-- |Applicative sequencing for RPC requests
module Ribosome.Host.Data.RpcCall where

import Data.MessagePack (Object)

import Ribosome.Host.Class.Msgpack.Error (DecodeError (DecodeError))
import Ribosome.Host.Data.Request (Request)

type DecodeAtomic a =
  [Object] -> Either DecodeError ([Object], a)

decodeAtomWith ::
  (Object -> Either DecodeError a) ->
  [Object] ->
  Either DecodeError ([Object], a)
decodeAtomWith decode = \case
  o : rest ->
    (rest,) <$> decode o
  [] ->
    Left (DecodeError "atomic call response" "Too few results")

type RpcCall :: Type -> Type

-- |A wrapper for 'Request' that allows applicative sequencing of calls for batch processing, used for a declarative
-- representation of the Neovim API.
--
-- Neovim has an API function named @nvim_call_atomic@ that makes it possible to send multiple RPC requests at once,
-- reducing the communication overhead.
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
  RpcPure :: a -> RpcCall a
  RpcRequest :: Request -> (Object -> Either DecodeError a) -> RpcCall a
  RpcBind :: RpcCall a -> (a -> RpcCall b) -> RpcCall b
  RpcAtomic :: [Request] -> DecodeAtomic a -> RpcCall a

instance Functor RpcCall where
  fmap f = \case
    RpcPure a ->
      RpcPure (f a)
    RpcRequest req decode ->
      RpcRequest req (fmap f . decode)
    RpcBind fa g ->
      RpcBind fa (fmap f . g)
    RpcAtomic reqs decode ->
      RpcAtomic reqs (fmap (second f) . decode)
  {-# inline fmap #-}

asAtomic :: RpcCall a -> RpcCall a
asAtomic = \case
  RpcRequest req decode ->
    RpcAtomic [req] (decodeAtomWith decode)
  RpcPure a ->
    RpcAtomic [] (Right . (,a))
  step ->
    step
{-# inline asAtomic #-}

instance Applicative RpcCall where
  pure =
    RpcPure

  liftA2 f (asAtomic -> RpcAtomic reqsA decodeA) (asAtomic -> RpcAtomic reqsB decodeB) =
    RpcAtomic (reqsA <> reqsB) \ o -> do
      (restA, a) <- decodeA o
      second (f a) <$> decodeB restA
  liftA2 f fa fb =
    fa >>= \ a ->
      f a <$> fb
  {-# inline liftA2 #-}

instance Monad RpcCall where
  (>>=) =
    RpcBind
  {-# inline (>>=) #-}

instance (
    Semigroup a
  ) => Semigroup (RpcCall a) where
    (<>) =
      liftA2 (<>)
    {-# inline (<>) #-}

instance (
    Monoid a
  ) => Monoid (RpcCall a) where
    mempty =
      pure mempty
    {-# inline mempty #-}
