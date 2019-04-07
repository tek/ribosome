module Ribosome.Plugin (
  module Ribosome.Plugin,
  rpcHandler,
  rpcHandlerDef,
  RpcHandlerConfig(..),
) where

import Control.Monad (join, (<=<))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.ByteString.UTF8 (fromString)
import Data.Default (def)
import qualified Data.Map as Map ()
import Data.Maybe (fromMaybe)
import Data.MessagePack (Object(ObjectNil))
import Neovim.Context (Neovim)
import Neovim.Plugin.Classes (
  AutocmdOptions,
  CommandOption,
  FunctionName(..),
  FunctionalityDescription(..),
  Synchronous(..),
  )
import Neovim.Plugin.Internal (ExportedFunctionality(..), Plugin(..))

import Ribosome.Plugin.TH (
  RpcDef(RpcDef),
  RpcDefDetail(..),
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
    wrap (RpcDef detail name' rpcHandler') =
      EF (wrapDetail detail (F (fromString name')), executeRpcHandler errorHandler rpcHandler')
    wrapDetail (RpcFunction sync') name' =
      Function name' sync'
    wrapDetail (RpcCommand options) name' =
      Command name' options
    wrapDetail (RpcAutocmd event sync options) name' =
      Autocmd (fromString event) name' sync options

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

autocmd :: String -> RpcHandlerConfig -> RpcHandlerConfig
autocmd event conf =
  conf { rhcAutocmd = Just event }

autocmdOptions :: AutocmdOptions -> RpcHandlerConfig -> RpcHandlerConfig
autocmdOptions options conf =
  conf { rhcAutocmdOptions = Just options }
