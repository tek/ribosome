-- |Main function combinators for connecting to Neovim over a socket.
module Ribosome.Socket where

import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.NvimSocket (NvimSocket)
import Ribosome.Host.Interpreter.Handlers (interpretHandlersNull)
import Ribosome.Host.Interpreter.Process.Socket (interpretProcessCerealSocket)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)
import Ribosome.IOStack (BasicPluginStack)
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Interpreter.VariableWatcher (interpretVariableWatcherNull)
import Ribosome.Run (PluginEffects)

-- |The stack of plugin internals.
type SocketHandlerEffects =
  PluginEffects ++ RpcStack ++ RpcDeps

-- |The complete stack of a Neovim plugin.
type PluginSocketStack c =
  SocketHandlerEffects ++ Reader NvimSocket : BasicPluginStack c

-- |Run plugin internals without IO effects.
interpretPluginSocket ::
  Members (BasicPluginStack c) r =>
  Member (Reader NvimSocket) r =>
  InterpretersFor SocketHandlerEffects r
interpretPluginSocket =
  interpretUserErrorPrefixed .
  interpretProcessCerealSocket def .
  resumeHoistError (BootError . show @Text) .
  raiseUnder .
  interpretRpcStack .
  interpretHandlersNull .
  interpretVariableWatcherNull .
  interpretSettingsRpc .
  interpretScratch
