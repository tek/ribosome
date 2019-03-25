module Ribosome.Plugin (
  module Ribosome.Plugin,
  rpcHandlerDef,
) where

import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.ByteString.UTF8 (fromString)
import qualified Data.Map as Map ()
import Data.MessagePack (Object)
import Neovim.Context (Neovim)
import Neovim.Plugin.Classes (
  FunctionName(..),
  FunctionalityDescription(..),
  )
import Neovim.Plugin.Internal (ExportedFunctionality(..), Plugin(..))

import Ribosome.Plugin.TH (RpcFunction(RpcFunction), rpcHandlerDef)

class RpcHandler e env m | m -> e env where
  native :: m a -> ExceptT e (Neovim env) a

instance RpcHandler e env (ExceptT e (Neovim env)) where
  native = id

class RpcErrorHandler e m where

nvimPlugin :: RpcHandler e env m => env -> [RpcFunction m] -> (e -> m ()) -> Plugin env
nvimPlugin env fs errorHandler =
  Plugin env (wrap <$> fs)
  where
    wrap (RpcFunction name sync rpcHandler) =
      EF (Function (F (fromString name)) sync, executeRpcHandler errorHandler rpcHandler)

executeRpcHandler ::
  ∀ e env m.
  RpcHandler e env m =>
  (e -> m ()) ->
  ([Object] -> m Object) ->
  [Object] ->
  Neovim env Object
executeRpcHandler errorHandler rpcHandler =
  either handleError return <=< runExceptT . native . rpcHandler
  where
    handleError e = do
      _ <- runExceptT $ native @e $ errorHandler e
      fail "fatal error in rpc handler"
