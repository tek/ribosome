module Ribosome.Plugin (
  module Ribosome.Plugin,
  rpcHandler,
  rpcHandlerDef,
  RpcHandlerConfig(..),
) where

import Control.Monad (join, (<=<))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.ByteString.UTF8 (fromString)
import qualified Data.Map as Map ()
import Data.MessagePack (Object(ObjectNil))
import Neovim.Context (Neovim)
import Neovim.Plugin.Classes (
  CommandOption,
  FunctionName(..),
  FunctionalityDescription(..),
  Synchronous(..),
  )
import Neovim.Plugin.Internal (ExportedFunctionality(..), Plugin(..))

import Ribosome.Plugin.TH (
  RpcDef(RpcDef),
  RpcDefDetail(RpcFunction, RpcCommand),
  RpcHandlerConfig(..),
  rhcCmd,
  rpcHandler,
  rpcHandlerDef,
  )

class RpcHandler e env m | m -> e env where
  native :: m a -> ExceptT e (Neovim env) a

instance RpcHandler e env (ExceptT e (Neovim env)) where
  native = id

nvimPlugin :: RpcHandler e env m => env -> [[RpcDef m]] -> (e -> m ()) -> Plugin env
nvimPlugin env fs errorHandler =
  Plugin env (wrap <$> join fs)
  where
    wrap (RpcDef (RpcFunction sync') name' rpcHandler') =
      EF (Function (F (fromString name')) sync', executeRpcHandler errorHandler rpcHandler')
    wrap (RpcDef (RpcCommand options) name' rpcHandler') =
      EF (Command (F (fromString name')) options, executeRpcHandler errorHandler rpcHandler')

executeRpcHandler ::
  ∀ e env m.
  RpcHandler e env m =>
  (e -> m ()) ->
  ([Object] -> m Object) ->
  [Object] ->
  Neovim env Object
executeRpcHandler errorHandler rpcHandler' =
  either handleError return <=< runExceptT . native . rpcHandler'
  where
    handleError e = do
      _ <- runExceptT $ native @e $ errorHandler e
      return ObjectNil

cmd :: [CommandOption] -> RpcHandlerConfig -> RpcHandlerConfig
cmd opts conf =
  conf { rhcCmd = Just opts }

sync :: RpcHandlerConfig -> RpcHandlerConfig
sync conf =
  conf { rhcSync = Sync }

name :: String -> RpcHandlerConfig -> RpcHandlerConfig
name n conf =
  conf { rhcName = Just n }
