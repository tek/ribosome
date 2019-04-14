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
import Data.Default (def)
import qualified Data.Map as Map ()
import Data.MessagePack (Object(ObjectNil))
import Neovim.Context (Neovim)
import Neovim.Plugin.Classes (
  AutocmdOptions(acmdGroup),
  CommandOption,
  FunctionName(..),
  FunctionalityDescription(..),
  Synchronous(..),
  )
import Neovim.Plugin.Internal (ExportedFunctionality(..), Plugin(..))

import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Mapping (MappingError)
import Ribosome.Data.Setting (Setting)
import Ribosome.Data.Text (capitalize)
import Ribosome.Plugin.Mapping (MappingHandler, handleMappingRequest)
import Ribosome.Plugin.RpcHandler (RpcHandler(..))
import Ribosome.Plugin.TH (
  RpcDef(RpcDef),
  RpcDefDetail(..),
  RpcHandlerConfig(..),
  rhcCmd,
  rpcHandler,
  rpcHandlerDef,
  )
import Ribosome.Plugin.Watch (handleWatcherRequest, watchedVariables)

mappingHandlerRpc ::
  MonadDeepError e MappingError m =>
  Text ->
  [MappingHandler m] ->
  RpcDef m
mappingHandlerRpc pluginName mappings =
  RpcDef (RpcFunction Async) (capitalize pluginName <> "Mapping") (handleMappingRequest mappings)

watcherRpc ::
  MonadDeepError e MappingError m =>
  MonadRibo m =>
  NvimE e m =>
  Text ->
  Map Text (m ()) ->
  RpcDef m
watcherRpc pluginName variables =
  RpcDef detail (capitalize pluginName <> "VariableChanged") (handleWatcherRequest (watchedVariables variables))
  where
    detail = RpcAutocmd "CmdlineLeave" Async def

compileRpcDef ::
  RpcHandler e env m =>
  (e -> m ()) ->
  RpcDef m ->
  ExportedFunctionality env
compileRpcDef errorHandler (RpcDef detail name' rpcHandler') =
  EF (wrapDetail detail (F (encodeUtf8 name')), executeRpcHandler errorHandler rpcHandler')
  where
    wrapDetail (RpcFunction sync') name' =
      Function name' sync'
    wrapDetail (RpcCommand options) name' =
      Command name' options
    wrapDetail (RpcAutocmd event sync' options) name' =
      Autocmd (encodeUtf8 event) name' sync' options

nvimPlugin ::
  MonadDeepError e MappingError m =>
  RpcHandler e env m =>
  Text ->
  env ->
  [[RpcDef m]] ->
  (e -> m ()) ->
  Plugin env
nvimPlugin pluginName env rpcDefs errorHandler =
  Plugin env (compileRpcDef errorHandler <$> join rpcDefs)

riboPlugin ::
  MonadDeepError e MappingError m =>
  MonadRibo m =>
  NvimE e m =>
  RpcHandler e env m =>
  Text ->
  env ->
  [[RpcDef m]] ->
  [MappingHandler m] ->
  (e -> m ()) ->
  Map Text (m ()) ->
  Plugin env
riboPlugin pluginName env rpcDefs mappings errorHandler variables =
  Plugin env ((compileRpcDef errorHandler <$> extra) ++ efs)
  where
    Plugin _ efs = nvimPlugin pluginName env rpcDefs errorHandler
    extra = [mappingHandlerRpc pluginName mappings, watcherRpc pluginName variables]

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
    handleError e =
      ObjectNil <$ (runExceptT . native @e . errorHandler $ e)

cmd :: [CommandOption] -> RpcHandlerConfig -> RpcHandlerConfig
cmd opts conf =
  conf { rhcCmd = Just opts }

sync :: RpcHandlerConfig -> RpcHandlerConfig
sync conf =
  conf { rhcSync = Sync }

name :: Text -> RpcHandlerConfig -> RpcHandlerConfig
name n conf =
  conf { rhcName = Just n }

autocmd :: Text -> RpcHandlerConfig -> RpcHandlerConfig
autocmd event conf =
  conf { rhcAutocmd = Just event }

autocmdOptions :: AutocmdOptions -> RpcHandlerConfig -> RpcHandlerConfig
autocmdOptions options conf =
  conf { rhcAutocmdOptions = Just options }
