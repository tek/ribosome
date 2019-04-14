module Ribosome.Plugin.RpcHandler where

import Control.Monad.Trans.Except (ExceptT)
import Neovim (Neovim)

class RpcHandler e env m | m -> e env where
  native :: m a -> ExceptT e (Neovim env) a

instance RpcHandler e env (ExceptT e (Neovim env)) where
  native = id
