-- Description: The low-level API to Ribosome's core logic
module Ribosome.Host (
  -- * Introduction
  -- $intro

  -- * Execution
  module Ribosome.Host.Embed,
  module Ribosome.Host.Remote,
  module Ribosome.Host.Data.HostConfig,

  -- * Handlers
  module Ribosome.Host.Data.RpcHandler,
  module Ribosome.Host.Data.RpcType,
  module Ribosome.Host.Data.RpcError,
  module Ribosome.Host.Handler,
  module Ribosome.Host.Data.Execution,
  module Ribosome.Host.Data.Args,
  module Ribosome.Host.Data.Bang,
  module Ribosome.Host.Data.Bar,
  module Ribosome.Host.Data.CommandMods,
  module Ribosome.Host.Data.CommandRegister,
  module Ribosome.Host.Data.Range,

  -- * Effects
  module Ribosome.Host.Effect.Handlers,
  module Ribosome.Host.Effect.Host,
  module Ribosome.Host.Effect.MState,
  module Ribosome.Host.Effect.Reports,
  module Ribosome.Host.Effect.Responses,
  module Ribosome.Host.Effect.Rpc,
  module Ribosome.Host.Effect.UserError,

  -- * Interpreters
  module Ribosome.Host.Interpreter.Handlers,
  module Ribosome.Host.Interpreter.Host,
  module Ribosome.Host.Interpreter.Log,
  module Ribosome.Host.Interpreter.MState,
  module Ribosome.Host.Interpreter.Reports,
  module Ribosome.Host.Interpreter.Responses,
  module Ribosome.Host.Interpreter.Rpc,
  module Ribosome.Host.Interpreter.UserError,

  -- * Neovim API
  module Ribosome.Host.Api.Data,

  -- * Messagepack
  module Ribosome.Host.Class.Msgpack.Decode,
  module Ribosome.Host.Class.Msgpack.Encode,
  module Ribosome.Host.Class.Msgpack.Array,
  module Ribosome.Host.Class.Msgpack.Map,

  -- * Errors
  module Ribosome.Host.Data.Report,
  module Ribosome.Host.Error,
  module Ribosome.Host.Data.BootError,
  module Ribosome.Host.Data.StoredReport,
) where

import Ribosome.Host.Api.Data (Buffer, Tabpage, Window)
import Ribosome.Host.Class.Msgpack.Array (msgpackArray)
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Host.Class.Msgpack.Map (msgpackMap)
import Ribosome.Host.Data.Args
import Ribosome.Host.Data.Bang (Bang (Bang, NoBang))
import Ribosome.Host.Data.Bar (Bar (Bar))
import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.CommandMods (CommandMods (CommandMods))
import Ribosome.Host.Data.CommandRegister (CommandRegister (CommandRegister))
import Ribosome.Host.Data.Execution (Execution (Async, Sync))
import Ribosome.Host.Data.HostConfig (HostConfig (..), LogConfig (..), setStderr)
import Ribosome.Host.Data.Range (Range (Range), RangeStyle (..))
import Ribosome.Host.Data.Report (
  LogReport (LogReport),
  Report (Report),
  ReportContext (..),
  Reportable (toReport),
  basicReport,
  mapReport,
  mapReports,
  mapUserMessage,
  resumeHoistUserMessage,
  resumeReport,
  resumeReports,
  toReport,
  userReport,
  )
import Ribosome.Host.Data.RpcError (RpcError, rpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler (RpcHandler), simpleHandler)
import Ribosome.Host.Data.RpcType (CompleteStyle (..))
import Ribosome.Host.Data.StoredReport (StoredReport (StoredReport))
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Effect.Host (Host)
import Ribosome.Host.Effect.MState (
  MState,
  ScopedMState,
  mmodify,
  mread,
  mreads,
  mstate,
  mtrans,
  muse,
  stateToMState,
  withMState,
  )

import Ribosome.Host.Effect.Reports (Reports)
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Effect.Rpc (Rpc, async, notify, sync)
import Ribosome.Host.Effect.UserError
import Ribosome.Host.Embed (embedNvim, embedNvim_, interpretHostEmbed, testHostEmbed, withHostEmbed)
import Ribosome.Host.Error (ignoreRpcError)
import Ribosome.Host.Handler (completeBuiltin, completeWith, rpc, rpcAutocmd, rpcCommand, rpcFunction)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers, noHandlers, withHandlers)
import Ribosome.Host.Interpreter.Host (HostDeps, interpretHost, runHost, testHost, withHost)
import Ribosome.Host.Interpreter.Log (interpretLogs)
import Ribosome.Host.Interpreter.MState (evalMState, interpretMState, interpretMStates)
import Ribosome.Host.Interpreter.Reports (interpretReports)
import Ribosome.Host.Interpreter.Responses (interpretResponses)
import Ribosome.Host.Interpreter.Rpc (interpretRpc)
import Ribosome.Host.Interpreter.UserError (interpretUserErrorInfo)
import Ribosome.Host.Remote (interpretHostRemote, runHostRemote, runHostRemoteIO)

-- $intro
-- This library is a framework for building [Neovim](https://neovim.io) plugins with
-- [Polysemy](https://hackage.haskell.org/package/polysemy).
--
-- This package is the low-level core of the Neovim plugin host and is not intended for authors who want to build full
-- plugins.
-- Please consult the documentation for the
-- [main package](https://hackage.haskell.org/package/ribosome/docs/Ribosome.html) instead.
