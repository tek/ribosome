module Ribosome.Api.Process where

import Control.Monad.DeepError (MonadDeepError)

import Ribosome.Control.Monad.Ribo (Nvim)
import Ribosome.Nvim.Api.IO (vimCallFunction)
import Ribosome.Nvim.Api.RpcCall (RpcError)

vimPid :: (MonadDeepError e RpcError m, Nvim m) => m Int
vimPid =
  vimCallFunction "getpid" []
