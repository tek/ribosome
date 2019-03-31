module Ribosome.Api.Atomic where

import Data.MessagePack (Object)
import Neovim.Plugin.Classes (FunctionName(F))

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Nvim.Api.IO (nvimCallAtomic)
import Ribosome.Nvim.Api.RpcCall (RpcCall(RpcCall))

atomic :: NvimE e m => [RpcCall] -> m [Object]
atomic calls =
  nvimCallAtomic (call <$> calls)
  where
    call (RpcCall (F name) args) = toMsgpack [toMsgpack name, toMsgpack args]
