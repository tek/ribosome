module Ribosome.Host.Effect.Rpc where

import Prelude hiding (async)

import Ribosome.Host.Data.ChannelId (ChannelId)
import Ribosome.Host.Data.RpcCall (RpcCall)
import Ribosome.Host.Data.RpcError (RpcError)

-- |This effect abstracts interaction with the Neovim RPC API.
-- An RPC call can either be a /request/ or a /notification/, where the former expects a response to be sent while the
-- latter returns immediately.
--
-- For requests, the constructor 'sync' blocks the current thread while 'async' takes a callback that is called from a
-- new thread.
--
-- The constructor 'notify' sends a notification.
--
-- The module [Ribosome.Api.Data]("Ribosome.Api.Data") contains 'RpcCall's for the entire Neovim API, generated by
-- calling @neovim --api-info@ during compilation from Template Haskell.
--
-- The module "Ribosome.Api" contains functions that call 'sync' with those 'RpcCall's, converting the input and return
-- values to and from msgpack.
--
-- These functions have signatures like:
--
-- > nvimGetVar :: ∀ a r . Member Rpc r => MsgpackDecode a => Text -> Sem r a
--
-- A manual call would be constructed like this:
--
-- > Ribosome.sync (RpcRequest (Request "nvim_get_option" [toMsgpack "textwidth"]))
--
-- RPC calls may be batched and sent via @nvim_call_atomic@, see 'RpcCall'.
--
-- This effect's default interpreter uses 'Resumable' for error tracking. See [Errors]("Ribosome#g:errors").
data Rpc :: Effect where
  -- |Block the current thread while sending an RPC request.
  Sync :: RpcCall a -> Rpc m a
  -- |Send an RPC request and pass the result to the continuation on a new thread.
  Async :: RpcCall a -> (Either RpcError a -> m ()) -> Rpc m ()
  -- |Send an RPC notification and return immediately.
  Notify :: RpcCall a -> Rpc m ()
  -- |The Neovim RPC channel ID
  ChannelId :: Rpc m ChannelId

makeSem_ ''Rpc

-- |Block the current thread while sending an RPC request.
sync ::
  ∀ r a .
  Member Rpc r =>
  RpcCall a ->
  Sem r a

-- |Send an RPC request and pass the result to the continuation on a new thread.
async ::
  ∀ a r .
  Member Rpc r =>
  RpcCall a ->
  (Either RpcError a -> Sem r ()) ->
  Sem r ()

-- |Send an RPC notification and return immediately.
notify ::
  ∀ a r .
  Member Rpc r =>
  RpcCall a ->
  Sem r ()

-- |The Neovim RPC channel ID
channelId ::
  ∀ r .
  Member Rpc r =>
  Sem r ChannelId
