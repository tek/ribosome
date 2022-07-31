module Ribosome (
  -- * Introduction
  -- $intro

  -- * Creating a project
  -- $project

  -- * Handlers
  -- $handlers

  -- ** Handler definition
  RpcHandler (..),
  Handler,
  RpcName (..),

  -- ** Constructing handlers
  rpcFunction,
  rpcCommand,
  rpcAutocmd,
  rpc,
  Execution (..),

  -- * Remote plugin execution #execution#
  -- $execution
  runNvimPluginIO,
  runNvimPluginIO_,
  withHandlers,
  remotePlugin,
  RemoteStack,
  runRemoteStack,
  interpretPluginRemote,
  BasicPluginStack,
  runBasicPluginStack,
  runCli,
  NvimPlugin,

  -- * Interacting with Neovim
  -- $api
  Rpc,
  Request (Request),
  RpcCall,
  sync,
  async,
  notify,
  Buffer,
  Window,
  Tabpage,

  -- * Watching variables
  -- $watched-variables
  watchVariables,
  WatchedVariable (..),

  -- * Embedded Neovim execution
  -- $embed
  runEmbedPluginIO,
  runEmbedPluginIO_,
  embedPlugin,
  runEmbedStack,
  interpretPluginEmbed,

  -- * MessagePack codec
  -- $msgpack
  MsgpackDecode (fromMsgpack),
  MsgpackEncode (toMsgpack),
  pattern Msgpack,
  msgpackArray,
  msgpackMap,

  -- * Utility effects
  -- $util

  -- ** Settings
  Settings,
  Setting (Setting),
  SettingError,
  interpretSettingsRpc,

  -- ** Scratch buffers
  -- $scratch
  Scratch,
  ScratchOptions,
  scratch,
  FloatOptions,
  ScratchId (ScratchId),
  ScratchState (ScratchState),

  -- ** Mappings
  -- $mappings
  Mapping (Mapping),
  MappingId (MappingId),
  MapMode (..),
  mappingFor,
  activateBufferMapping,
  activateMapping,

  -- ** Persisting data across vim sessions
  Persist,
  interpretPersist,
  interpretPersistNull,
  PersistPath,
  persistPath,
  interpretPersistPath,
  interpretPersistPathSetting,
  interpretPersistPathAt,
  PersistError,
  PersistPathError,
  -- ** The plugin's name
  PluginName (PluginName),
  interpretPluginName,

  -- * More functionality for handlers

  -- ** Command completion
  completeWith,
  completeBuiltin,
  CompleteStyle (..),

  -- ** Special command parameter types #command-params#
  HandlerArg (handlerArg),
  CommandHandler (commandOptions),
  Args (..),
  ArgList (..),
  JsonArgs (..),
  Options (..),
  OptionParser (..),
  Bang (..),
  Bar (..),
  Range (Range),
  RangeStyle (..),
  CommandMods (..),
  CommandRegister (..),
  HandlerCodec (handlerCodec),

  -- * Command Modifiers
  modifyCmd,
  bufdo,
  windo,
  noautocmd,
  silent,
  silentBang,

  -- * Configuring the host
  HostConfig (..),
  LogConfig (..),
  setStderr,
  PluginConfig (PluginConfig),
  pluginNamed,

  -- * Reports
  -- $errors
  resumeReport,
  mapReport,
  resumeReports,
  mapReports,
  LogReport (LogReport),
  Report (Report),
  Reportable (toReport),
  ReportContext (..),
  reportContext,
  prefixReportContext,
  reportContext',
  prefixReportContext',
  basicReport,
  userReport,
  reportMessages,
  resumeHoistUserMessage,
  mapUserMessage,
  logReport,
  pluginLogReports,
  RpcError,
  rpcError,
  ignoreRpcError,
  onRpcError,
  BootError (..),
  StoredReport (..),
  Reports,
  storedReports,
  reportStop,
  resumeLogReport,
  UserError,
  interpretUserErrorPrefixed,

  -- * Mutex State
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
  evalMState,
  interpretMState,
  interpretMStates,

  -- * Misc
  simpleHandler,
  noHandlers,
  interpretHandlers,
  Register,
  RegisterType,
  registerRepr,
  pathText,

  -- * Reexports
  module Prelate.Prelude,
) where

import Prelate.Prelude (Stop, type (!!), (<!))
import Prelude hiding (async)

import Ribosome.Data.FloatOptions (FloatOptions)
import Ribosome.Data.Mapping (MapMode (..), Mapping (Mapping), MappingId (MappingId))
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.PersistPathError (PersistPathError)
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig), pluginNamed)
import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Data.Register (Register, registerRepr)
import Ribosome.Data.RegisterType (RegisterType)
import Ribosome.Data.ScratchId (ScratchId (ScratchId))
import Ribosome.Data.ScratchOptions (ScratchOptions, scratch)
import Ribosome.Data.ScratchState (ScratchState (ScratchState))
import Ribosome.Data.Setting (Setting (Setting))
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Data.WatchedVariable (WatchedVariable (..))
import Ribosome.Effect.Persist (Persist)
import Ribosome.Effect.PersistPath (PersistPath, persistPath)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Embed (embedPlugin, interpretPluginEmbed, runEmbedPluginIO, runEmbedPluginIO_, runEmbedStack)
import Ribosome.Host.Api.Data (Buffer, Tabpage, Window)
import Ribosome.Host.Class.Msgpack.Array (msgpackArray)
import Ribosome.Host.Class.Msgpack.Decode (pattern Msgpack, MsgpackDecode (fromMsgpack))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Host.Class.Msgpack.Map (msgpackMap)
import Ribosome.Host.Data.Args (ArgList (..), Args (..), JsonArgs (..), OptionParser (..), Options (..))
import Ribosome.Host.Data.Bang (Bang (..))
import Ribosome.Host.Data.Bar (Bar (Bar))
import Ribosome.Host.Data.BootError (BootError (..))
import Ribosome.Host.Data.CommandMods (CommandMods (CommandMods))
import Ribosome.Host.Data.CommandRegister (CommandRegister (CommandRegister))
import Ribosome.Host.Data.Execution (Execution (..))
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
  prefixReportContext,
  prefixReportContext',
  reportContext,
  reportContext',
  reportMessages,
  resumeHoistUserMessage,
  resumeReport,
  resumeReports,
  toReport,
  userReport,
  )
import Ribosome.Host.Data.Request (Request (Request))
import Ribosome.Host.Data.RpcCall (RpcCall)
import Ribosome.Host.Data.RpcError (RpcError, rpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler (..), simpleHandler)
import Ribosome.Host.Data.RpcName (RpcName (..))
import Ribosome.Host.Data.RpcType (CompleteStyle (..))
import Ribosome.Host.Data.StoredReport (StoredReport (StoredReport))
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
import Ribosome.Host.Effect.Reports (Reports, storedReports)
import Ribosome.Host.Effect.Rpc (Rpc, async, notify, sync)
import Ribosome.Host.Effect.UserError (UserError)
import Ribosome.Host.Error (ignoreRpcError, onRpcError)
import Ribosome.Host.Handler (
  completeBuiltin,
  completeWith,
  rpc,
  rpcAutocmd,
  rpcCommand,
  rpcFunction,
  )
import Ribosome.Host.Handler.Codec (HandlerArg (handlerArg), HandlerCodec (handlerCodec))
import Ribosome.Host.Handler.Command (CommandHandler (commandOptions))
import Ribosome.Host.Interpreter.Handlers (interpretHandlers, noHandlers, withHandlers)
import Ribosome.Host.Interpreter.MState (evalMState, interpretMState, interpretMStates)
import Ribosome.Host.Modify (bufdo, modifyCmd, noautocmd, silent, silentBang, windo)
import Ribosome.IOStack (BasicPluginStack, runBasicPluginStack, runCli)
import Ribosome.Interpreter.Persist (interpretPersist, interpretPersistNull)
import Ribosome.Interpreter.PersistPath (interpretPersistPath, interpretPersistPathAt, interpretPersistPathSetting)
import Ribosome.Interpreter.PluginName
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Interpreter.VariableWatcher (watchVariables)
import Ribosome.Mapping (activateBufferMapping, activateMapping, mappingFor)
import Ribosome.Path (pathText)
import Ribosome.Remote (
  RemoteStack,
  interpretPluginRemote,
  remotePlugin,
  runNvimPluginIO,
  runNvimPluginIO_,
  runRemoteStack,
  )
import Ribosome.Report (logReport, pluginLogReports, reportStop, resumeLogReport)
import Ribosome.Run (NvimPlugin)

-- $intro
-- This library is a framework for building [Neovim](https://neovim.io) plugins with
-- [Polysemy](https://hackage.haskell.org/package/polysemy).
--
-- A plugin consists of a set of request handlers that can be executed by Neovim functions, commands, autocmds, or
-- events, and may communicate with Neovim by calling its RPC API.
--
-- Here is an example for a simple plugin with a single request handler.
--
-- > import Ribosome
-- > import Ribosome.Api
-- >
-- > count ::
-- >   Members NvimPlugin r =>
-- >   Int ->
-- >   Handler r Int
-- > count n = do
-- >   s <- 0 <! nvimGetVar "sum"
-- >   let s' = s + n
-- >   ignoreRpcError (nvimSetVar "sum" s')
-- >   pure s'
-- >
-- > main :: IO ()
-- > main =
-- >   runNvimPluginIO_ "counter" [rpcFunction "Count" Sync count]
--
-- This module can be used as a Neovim plugin by running it with @jobstart@ from Neovim:
--
-- > :call jobstart(['/path/to/plugin.exe'], { 'rpc': 1 })
--
-- The handler will add up all numbers that are passed to the Neovim function @Count@ and store the sum in the variable
-- @g:sum@:
--
-- > :echo Count(5)
-- > 5
-- > :echo Count(13)
-- > 18
-- > :echo g:sum
-- > 18

-- $project
-- The most reliable way to set up a repository for a plugin is to use Nix, for which Ribosome provides an app that
-- generates a ready-to-use plugin project that includes Neovim glue that fetches static binaries from Github, as well
-- as config files for Github Actions that release those binaries for every commit and tag:
--
-- > $ nix run 'github:tek/ribosome#new' my-plugin
--
-- The created plugin can be added to Neovim like any other.
-- For example, linking its directory to @~\/.local\/share\/nvim\/site\/pack\/foo\/opt\/my-plugin@ will allow you to
-- run:
--
-- > :packadd my-plugin
--
-- Using @start@ instead of @opt@ in the pack path will run the plugin at startup.
--
-- Or simply use one of the many plugin managers.
--
-- On the first start, the plugin will either be built with Nix, if it is available, or a static binary will be fetched
-- from Github.
-- Once that is done, the template project's dummy handler can be executed:
--
-- > :echo MyPluginPing()
-- > 0
-- > :echo MyPluginPing()
-- > 1
--
-- The second time the plugin ist started, the executable will be run directly, without checking for updates, unless the
-- result has been garbage collected by Nix (i.e. the @result@ link in the repo is broken).
-- In order to force a rebuild after pulling, run the command:
--
-- > $ nix build

-- $handlers
-- A list of 'RpcHandler's can be created by passing a handler function to one the smart constructors:
--
-- > echoHello :: Member (Rpc !! RpcError) => Sem r ()
-- > echoHello = ignoreRpcError (echo "Hello")
-- >
-- > handlers = [
-- >   rpcFunction "Hello" Async echoHello,
-- >   rpcCommand "Hello" Async echoHello,
-- >   rpcAutocmd "HelloHaskellFile" Async "BufEnter" "*.hs" echoHello
-- > ]
--
-- Passing these handlers to 'runNvimPluginIO_' starts a plugin that calls @echoHello@ when running @:call Hello()@,
-- @:Hello@, or when entering a Haskell buffer.
--
-- When the plugin's main loop starts, 'interpretHandlers' registers the triggers in Neovim by running vim code like
-- this:
--
-- > function! Hello(...) range
-- >   return call('rpcnotify', [1, 'function:Hello'] + a:000)
-- > endfunction
-- > command! -nargs=0 Hello call call('rpcnotify', [1, 'command:Hello'])
-- > autocmd BufEnter *.hs call call('rpcnotify', [1, 'autocmd:HelloHaskellFile'])

-- $execution
-- There are many ways of running a plugin for different purposes, like as a remote plugin from Neovim (the usual
-- production mode), directly in a test using an embedded Neovim process, or over a socket when testing a plugin in
-- tmux.

-- $watched-variables
-- /Watched variable handlers/ are called whenever a certain Neovim variable's value has changed:
--
-- > changed ::
-- >   Members NvimPlugin r =>
-- >   Object ->
-- >   Handler r ()
-- > changed value =
-- >   ignoreRpcError (echo ("Changed value to: " <> show value))
-- >
-- > main :: IO ()
-- > main = runRemoteStack "watch-plugin" (watchVariables [("trigger", changed)] remotePlugin)
--
-- This registers the variable named @trigger@ to be watched for changes.
-- When a change is detected, the handler @changed@ whill be executed with the new value as its argument.
--
-- /Note/ that the combinators in the main function are simply what's run by 'runNvimPluginIO', with 'watchVariables'
-- being used as the custom effect stack and an empty list of handlers.

-- $api
--
-- - The effect 'Rpc' governs access to Neovim's remote API.
--
-- - The module [Ribosome.Api.Data]("Ribosome.Api.Data") contains declarative representations of all API calls that are
-- listed at @:help api@.
--
-- - The module [Ribosome.Api.Effect]("Ribosome.Api.Effect"), reexported from [Ribosome.Api]("Ribosome.Api"), contains
-- the same set of API functions, but as callable 'Sem' functions that use the data declarations with 'sync'.
-- [Ribosome.Api]("Ribosome.Api") additionally contains many composite functions using the Neovim API.
--
-- The API also defines the data types 'Buffer', 'Window' and 'Tabpage', which are abstract types carrying an internal
-- identifier generated by Neovim.

-- $embed
-- While [remote plugins]("Ribosome#execution") are executed from within Neovim, Ribosome can also run Neovim from a
-- Haskell process and attach to the subprocess' stdio.
--
-- The primary purpose of embedding Neovim is testing a plugin, but it could also be used to build a GUI application
-- around Neovim.
--
-- The library [Ribosome.Test](https://hackage.haskell.org/package/ribosome-test/docs/Ribosome-Test.html) provides more
-- comprehensive functionality for the testing use case.
--
-- When embedding Neovim, the main loop is forked and the test is run synchronously:
--
-- > import qualified Data.Text.IO as Text
-- > import Ribosome
-- > import Ribosome.Api
-- >
-- > ping :: Handler r Text
-- > ping = pure "Ping"
-- >
-- > main :: IO ()
-- > main =
-- >   runEmbedPluginIO_ "ping-plugin" [rpcFunction "Ping" Sync ping] do
-- >     ignoreRpcError do
-- >       embed . Text.putStrLn =<< nvimCallFunction "Ping" []

-- $msgpack
-- Neovim's RPC communication uses the MessagePack protocol.
-- All API functions convert their arguments and return values using the classes 'MsgpackEncode' and 'MsgpackDecode'.
-- There are several Haskell libraries for this purpose.
-- Ribosome uses [messagepack](https://hackage.haskell.org/package/messagepack), simply for the reason that it allows
-- easy incremental parsing via [cereal](https://hackage.haskell.org/package/cereal).
--
-- All API functions that are declared as taking or returning an 'Data.MessagePack.Object' by Neovim are kept
-- polymorphic, allowing the user to interface with them using arbitrary types.
-- Codec classes for record types can be derived generically:
--
-- > data Cat =
-- >   Cat { name :: Text, age :: Int }
-- >   deriving stock (Generic)
-- >   deriving anyclass (MsgpackEncode, MsgpackDecode)
-- >
-- > nvimSetVar "cat" (Cat "Dr. Boots" 4)

-- $util
-- TODO

-- $scratch
-- A scratch buffer is what Neovim calls text not associated with a file, used for informational or interactive content.
-- Ribosome provides an interface for maintaining those, by associating a view configuration with an ID and allowing to
-- update the text displayed in it.
-- Its full API is exposed by [Ribosome.Scratch]("Ribosome.Scratch").

-- $mappings
-- The function 'activateBufferMapping' can be used to dynamically create buffer-local Neovim key mappings that trigger
-- handlers of a Ribosome plugin.
--
-- A slightly reliable way of constructing a 'Mapping' is to use 'mappingFor', which takes an 'RpcHandler' to ensure
-- that the name it calls was at least associated with a handler at some point.
--
-- One use case for mappings is in a 'Scratch' buffer, which automatically registers a set of them after initializing
-- the buffer.

-- $errors
-- Ribosome uses
-- [polysemy-resume](https://hackage.haskell.org/package/polysemy-resume/docs/Polysemy-Resume.html)
-- extensively, which is a concept for tracking errors across interpreters by attaching them to a wrapper effect.
--
-- In short, when an interpreter is written for the effect @'Rpc' !! 'RpcError'@ (which is a symbolic alias for
-- @'Resumable' 'RpcError' 'Rpc'@), every use of the bare effect 'Rpc' must be converted at some point, with the
-- possiblity of exposing the error on another interpreter that uses the effect.
--
-- Take the effect 'Scratch' for example, whose interpreter is for the effect @'Scratch' !! 'RpcError'@.
-- In there is the expression:
--
-- > restop @RpcError @Rpc (setScratchContent s text)
--
-- The function 'setScratchContent' has a dependency on the bare effect 'Rpc'.
-- The function 'restop' converts this dependency into @'Rpc' !! 'RpcError'@ /and/ @'Stop' 'RpcError'@, meaning that
-- this expression acknowledges that 'Rpc' might fail with 'RpcError', and rethrows the error, which is then turned into
-- @'Scratch' !! 'RpcError'@ by the special interpreter combinator 'interpretResumable'.
--
-- Instead of rethrowing, the error can also be caught, by using a combinator like 'resume' or the operator '<!' that is
-- similar to '<$'.
--
-- The concept is similar to 'Error', with the difference that a 'Resumable' interpreter can communicate that it throws
-- this type of error, while with plain 'Error', this would have to be tracked manually by the developer.
--
-- Since handler functions yield the control flow to Ribosome's internal machinery when returning, all 'Stop' effects
-- have to be converted to 'Report' (which is expected by the request dispatcher and part of the 'Handler' stack),
-- and all bare effects like 'Rpc' have to be resumed or restopped since their interpreters only operate on the
-- 'Resumable' variants.
--
-- To make this chore a little less verbose, the class 'Reportable' can be leveraged to convert errors to
-- 'Report', which consists of an 'Report' and 'ReportContext', which optionally identifies the plugin
-- component that threw the error.
--
-- Since 'RpcError' is an instance of 'Reportable', the combinators 'resumeReport' and 'mapReport' can be used to
-- reinterpret to @'Stop' 'Report'@.
