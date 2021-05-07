module Ribosome.Api.Process where

import Ribosome.Control.Monad.Ribo (Nvim)
import Ribosome.Nvim.Api.IO (vimCallFunction)
import Ribosome.Nvim.Api.RpcCall (RpcError)

vimPid :: (MonadDeepError e RpcError m, Nvim m) => m Int
vimPid =
  vimCallFunction "getpid" []
