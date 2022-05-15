module Ribosome.Plugin (
  module Ribosome.Plugin,
  rpcHandler,
  rpcHandlerDef,
  RpcHandlerConfig(..),
  RpcDef(..),
) where

import qualified Data.Map.Strict as Map ()
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

import Ribosome.Data.Mapping (MappingError)
import Ribosome.Data.Text (capitalize)
import Ribosome.Plugin.TH (rpcHandler, rpcHandlerDef)
import Ribosome.Plugin.Builtin (deleteScratchRpc)
import Ribosome.Plugin.Mapping (MappingHandler, handleMappingRequest)
import Ribosome.Plugin.RpcHandler (RpcHandler(..))
import Ribosome.Plugin.TH.Handler (
  RpcDef(RpcDef),
  RpcDefDetail(..),
  RpcHandlerConfig(..),
  rhcCmd,
  )
import Ribosome.Plugin.Watch (handleWatcherRequestSafe, watchedVariables)

poll ::
  Monad m =>
  [Object] ->
  m Object
poll _ =
  pure (ObjectBool True)

pollRpc ::
  Text ->
  RpcDef m
pollRpc pluginName =
  RpcDef (RpcFunction Sync) (capitalize pluginName <> "Poll") poll

mappingHandlerRpc ::
  Text ->
  [MappingHandler m] ->
  RpcDef m
mappingHandlerRpc pluginName mappings =
  RpcDef (RpcFunction Async) (capitalize pluginName <> "Mapping") (handleMappingRequest mappings)

watcherRpc ::
  Member Rpc r =>
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
      RpcDef dt (name' event) (handleWatcherRequestSafe (watchedVariables variables))
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
  RpcHandler e env m =>
  env ->
  [[RpcDef m]] ->
  (e -> m ()) ->
  Plugin env
nvimPlugin env rpcDefs errorHandler =
  Plugin env (compileRpcDef errorHandler <$> join rpcDefs)

riboPlugin ::
  Member Rpc r =>
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
  either handleError pure <=< runExceptT . native . rpcHandler'
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
