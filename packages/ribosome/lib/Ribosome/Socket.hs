module Ribosome.Socket where

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.NvimSocket (NvimSocket)
import Ribosome.Host.Interpreter.Handlers (interpretHandlersNull)
import Ribosome.Host.Interpreter.Host (withHost)
import Ribosome.Host.Interpreter.Process.Socket (interpretProcessCerealSocket)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)
import Ribosome.IOStack (BasicPluginStack)
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Interpreter.VariableWatcher (interpretVariableWatcherNull)
import Ribosome.Plugin.Builtin (interceptHandlersBuiltin)
import Ribosome.Run (PluginEffects)

type SocketHandlerEffects =
  PluginEffects ++ RpcStack ++ RpcDeps

type PluginSocketStack =
  SocketHandlerEffects ++ Reader NvimSocket : BasicPluginStack

interpretRpcDeps ::
  Members [Reader NvimSocket, Reader PluginName, Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  InterpretersFor RpcDeps r
interpretRpcDeps =
  interpretUserErrorPrefixed .
  interpretProcessCerealSocket def .
  resumeHoistError (BootError . show @Text) .
  raiseUnder

interpretPluginSocket ::
  Members BasicPluginStack r =>
  Member (Reader NvimSocket) r =>
  InterpretersFor SocketHandlerEffects r
interpretPluginSocket =
  interpretRpcDeps .
  interpretRpcStack .
  interpretHandlersNull .
  interpretVariableWatcherNull .
  interpretSettingsRpc .
  interpretScratch

-- |Fork the main loop for a plugin connected to Neovim over a socket.
socketPlugin ::
  Members BasicPluginStack r =>
  Members SocketHandlerEffects r =>
  Sem r a ->
  Sem r a
socketPlugin =
  interceptHandlersBuiltin .
  withHost .
  insertAt @0
