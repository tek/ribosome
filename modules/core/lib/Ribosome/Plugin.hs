module Ribosome.Plugin (
  module Ribosome.Plugin,
  rpcHandler,
  rpcHandlerDef,
  RpcHandlerConfig(..),
  RpcDef(..),
) where

import Control.Monad (join, (<=<))
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.ByteString.UTF8 (fromString)
import qualified Data.Map as Map ()
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

import Ribosome.Data.Mapping (MappingError)
import Ribosome.Data.String (capitalize)
import Ribosome.Plugin.Mapping (MappingHandler, handleMappingRequest)
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

mappingHandlerRpc ::
  MonadDeepError e MappingError m =>
  String ->
  [MappingHandler m] ->
  RpcDef m
mappingHandlerRpc pluginName mappings =
  RpcDef (RpcFunction Async) (capitalize pluginName ++ "Mapping") (handleMappingRequest mappings)

nvimPlugin ::
  MonadDeepError e MappingError m =>
  RpcHandler e env m =>
  String ->
  env ->
  [[RpcDef m]] ->
  [MappingHandler m] ->
  (e -> m ()) ->
  Plugin env
nvimPlugin pluginName env fs mappings errorHandler =
  Plugin env (wrap (mappingHandlerRpc pluginName mappings) : (wrap <$> join fs))
  where
    wrap (RpcDef detail name' rpcHandler') =
      EF (wrapDetail detail (F (fromString name')), executeRpcHandler errorHandler rpcHandler')
    wrapDetail (RpcFunction sync') name' =
      Function name' sync'
    wrapDetail (RpcCommand options) name' =
      Command name' options
    wrapDetail (RpcAutocmd event sync' options) name' =
      Autocmd (fromString event) name' sync' options

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
