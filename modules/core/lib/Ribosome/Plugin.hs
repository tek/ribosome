module Ribosome.Plugin (
  module Ribosome.Plugin,
  rpcHandler,
  rpcHandlerDef,
  RpcHandlerConfig(..),
  RpcDef(..),
) where

import Control.Monad (join, (<=<))
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.Trans.Except (runExceptT)
import Data.Default (def)
import qualified Data.Map as Map ()
import Data.MessagePack (Object(ObjectNil, ObjectBool))
import Neovim.Context (Neovim)
import Neovim.Plugin.Classes (
  AutocmdOptions(acmdPattern),
  CommandOption,
  FunctionName(..),
  FunctionalityDescription(..),
  Synchronous(..),
  )
import Neovim.Plugin.Internal (ExportedFunctionality(..), Plugin(..))

import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Mapping (MappingError)
import Ribosome.Data.Text (capitalize)
import Ribosome.Plugin.Builtin (deleteScratchRpc)
import Ribosome.Plugin.Mapping (MappingHandler, handleMappingRequest)
import Ribosome.Plugin.RpcHandler (RpcHandler(..))
import Ribosome.Plugin.TH (rpcHandler, rpcHandlerDef)
import Ribosome.Plugin.TH.Handler (
  RpcDef(RpcDef),
  RpcDefDetail(..),
  RpcHandlerConfig(..),
  rhcCmd,
  )
import Ribosome.Plugin.Watch (handleWatcherRequest, watchedVariables)

poll ::
  Monad m =>
  [Object] ->
  m Object
poll _ =
  return (ObjectBool True)

pollRpc ::
  MonadDeepError e MappingError m =>
  Text ->
  RpcDef m
pollRpc pluginName =
  RpcDef (RpcFunction Sync) (capitalize pluginName <> "Poll") poll

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
  Map Text (Object -> m ()) ->
  [RpcDef m]
watcherRpc pluginName variables =
  chromatinInit : (rpcDef <$> ["CmdlineLeave", "BufWinEnter", "VimEnter"])
  where
    chromatinInit = rpcDefFromDetail ((detail "User") { raOptions = def { acmdPattern = toString ciName } }) ciName
    ciName = "ChromatinInitialized"
    rpcDef event =
      rpcDefFromDetail (detail event) event
    rpcDefFromDetail dt event =
      RpcDef dt (name' event) (handleWatcherRequest (watchedVariables variables))
    name' event = capitalize pluginName <> "VariableChanged" <> event
    detail event = RpcAutocmd event Async def

compileRpcDef ::
  RpcHandler e env m =>
  (e -> m ()) ->
  RpcDef m ->
  ExportedFunctionality env
compileRpcDef errorHandler (RpcDef detail name' rpcHandler') =
  EF (wrapDetail detail (F (encodeUtf8 name')), executeRpcHandler errorHandler rpcHandler')
  where
    wrapDetail (RpcFunction sync') n =
      Function n sync'
    wrapDetail (RpcCommand options) n =
      Command n options
    wrapDetail (RpcAutocmd event sync' options) n =
      Autocmd (encodeUtf8 event) n sync' options

nvimPlugin ::
  MonadDeepError e MappingError m =>
  RpcHandler e env m =>
  env ->
  [[RpcDef m]] ->
  (e -> m ()) ->
  Plugin env
nvimPlugin env rpcDefs errorHandler =
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
  Map Text (Object -> m ()) ->
  Plugin env
riboPlugin pluginName env rpcDefs mappings errorHandler variables =
  Plugin env ((compileRpcDef errorHandler <$> extra) ++ efs)
  where
    Plugin _ efs = nvimPlugin env rpcDefs errorHandler
    extra = deleteScratchRpc pluginName : pollRpc pluginName : mappingHandlerRpc pluginName mappings : watch
    watch = watcherRpc pluginName variables

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
