module Ribosome.Host (
  Rpc,
  RpcError (RpcError),
  sync,
  async,
  notify,
  runNvimPlugin,
) where

import Prelude hiding (async)

import Ribosome.Host.Data.RpcError (RpcError (RpcError))
import Ribosome.Host.Effect.Rpc (Rpc, async, notify, sync)
import Ribosome.Host.Remote (runNvimPlugin)
