{-# options_haddock prune #-}

module Ribosome (
  -- * Introduction
  -- $intro
  -- * Creating a project
  -- $project

  -- * Handlers
  -- $handlers

  RpcHandler (..),
  Handler,

  -- ** Constructing handlers

  rpcFunction,
  rpcCommand,
  rpcAutocmd,

  -- * Plugin execution #execution#
  -- $execution

  -- ** Only handlers

  runNvimHandlersIO,
  runNvimHandlersIO_,
  RemoteStack,

  -- ** With mappings and watched variables

  runNvimPluginIO,
  runNvimPluginIO_,
  NvimPlugin,
  NvimPluginEffects,
  interpretNvimPlugin,

  -- * Interacting with Neovim
  -- $api

  Rpc,
  RpcCall,
  sync,
  async,
  notify,
  Buffer,
  Window,
  Tabpage,

  -- * Embedded testing

  -- $embed

  embedNvimPlugin,
  embedNvimPlugin_,
  interpretPluginEmbed,
  withPluginEmbed,

  -- * Utility effects
  Settings,
  interpretSettingsRpc,
  Scratch,
  interpretScratch,
  FloatOptions (FloatOptions),
  Persist,
  interpretPersist,
  interpretPersistNull,
  PersistPath,
  persistPath,
  interpretPersistPath,
  interpretPersistPathSetting,
  interpretPersistPathAt,
  PluginName (PluginName),
  interpretPluginName,

  -- * More functionality for handlers

  -- ** Command completion

  completeWith,

  -- ** Special command parameter types #command-params#
  Args (Args),

  -- * Errors
  -- $errors

  resumeHandlerError,
  mapHandlerError,
  resumeHandlerErrorFrom,
  mapHandlerErrorFrom,
  handlerError,
  basicHandlerError,
  resumeHandlerErrors,
  mapHandlerErrors,
  userErrorMessage,
  resumeHoistUserMessage,
  mapUserMessage,

  -- * Misc

  noHandlers,
  rpcHandlers,

  module Ribosome.Data.FloatOptions,
  module Ribosome.Data.Mapping,
  module Ribosome.Data.PersistError,
  module Ribosome.Data.PersistPathError,
  module Ribosome.Data.PluginConfig,
  module Ribosome.Data.Register,
  module Ribosome.Data.RegisterType,
  module Ribosome.Data.ScratchId,
  module Ribosome.Data.ScratchOptions,
  module Ribosome.Data.ScratchState,
  module Ribosome.Data.Setting,
  module Ribosome.Data.SettingError,
  module Ribosome.Data.WatchedVariable,
  module Ribosome.Interpreter.BuiltinHandlers,
  module Ribosome.Interpreter.MappingHandler,
  module Ribosome.Interpreter.UserError,
  module Ribosome.Interpreter.VariableWatcher,
  module Ribosome.Errors,
  module Ribosome.Host,
  module Ribosome.IOStack,
  module Ribosome.Path,
  module Prelate.Prelude,
) where

import Prelate.Prelude (type (!!), (<!))
import Prelude hiding (async)

import Ribosome.Data.FloatOptions (FloatOptions (FloatOptions))
import Ribosome.Data.Mapping (Mapping (Mapping), MappingIdent (MappingIdent))
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.PersistPathError (PersistPathError)
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig), pluginNamed)
import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Data.Register (Register, registerRepr)
import Ribosome.Data.RegisterType (RegisterType)
import Ribosome.Data.ScratchId (ScratchId (ScratchId))
import Ribosome.Data.ScratchOptions (ScratchOptions (ScratchOptions), scratch)
import Ribosome.Data.ScratchState (ScratchState (ScratchState))
import Ribosome.Data.Setting (Setting (Setting))
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Data.WatchedVariable (WatchedVariable (..))
import Ribosome.Effect.NvimPlugin (NvimPlugin, NvimPluginEffects)
import Ribosome.Effect.Persist (Persist)
import Ribosome.Effect.PersistPath (PersistPath, persistPath)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Embed (embedNvimPlugin, embedNvimPlugin_, interpretPluginEmbed, withPluginEmbed)
import Ribosome.Errors (reportError, reportStop, resumeReportError, storeError)
import Ribosome.Host
import Ribosome.Host.Data.Args (Args (Args))
import Ribosome.Host.Data.HandlerError (mapHandlerErrorFrom)
import Ribosome.Host.Data.RpcCall (RpcCall)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler (..))
import Ribosome.Host.Handler (completeWith, rpcAutocmd, rpcCommand, rpcFunction)
import Ribosome.IOStack (BasicPluginStack, runBasicPluginStack)
import Ribosome.Interpreter.BuiltinHandlers
import Ribosome.Interpreter.MappingHandler (interpretMappingHandler, interpretMappingHandlerNull)
import Ribosome.Interpreter.NvimPlugin (
  interpretNvimPlugin,
  noHandlers,
  rpcHandlers,
  )
import Ribosome.Interpreter.Persist (interpretPersist, interpretPersistNull)
import Ribosome.Interpreter.PersistPath (interpretPersistPath, interpretPersistPathAt, interpretPersistPathSetting)
import Ribosome.Interpreter.PluginName
import Ribosome.Interpreter.Scratch
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Interpreter.VariableWatcher (interpretVariableWatcher, interpretVariableWatcherNull)
import Ribosome.Path (pathText)
import Ribosome.Remote (RemoteStack, runNvimHandlersIO, runNvimHandlersIO_, runNvimPluginIO, runNvimPluginIO_)

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
-- >   Member (Rpc !! RpcError) r =>
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
-- >   runNvimHandlersIO_ (PluginConfig "counter" def) [rpcFunction "Count" Sync count]
--
-- This app can be used as a plugin by running it with @jobstart@ from Neovim:
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
-- The most reliable way to set up a repository for a plugin is to use the Nix, for which Ribosome provides an app that
-- generates a ready-to-use plugin project that includes Neovim glue that fetches static binaries from Github, as well
-- as config files for Github Actions that release those binaries for every commit and tag:
--
-- > $ nix run github:tek/ribosome#new my-plugin
--
-- The created plugin can be added to Neovim like any other.
-- For example, linking its directory to @~/.local/share/nvim/site/pack/foo/opt/my-plugin@ will allow you to run:
--
-- > :packadd my-plugin
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
-- A plugin consists of a set of request handlers, which can be triggered by functions, commands or autocommands.
-- When the main loop is started, all specified handlers are registered in Neovim.
-- See [Execution]("Ribosome#execution") for how to start a plugin with a set of handlers.

-- $execution
-- There are many ways of running a plugin for different purposes, like as a remote plugin from Neovim (the usual
-- production mode), directly in a test using an embedded Neovim process, or through a socket when testing a plugin in
-- tmux.

-- $api
-- The effect 'Rpc' governs access to Neovim's remote API.
--
-- The module [Ribosome.Api.Data]("Ribosome.Api.Data") contains declarative representations of all API calls that are
-- listed at @:help api@.
--
-- The module [Ribosome.Api.Effect]("Ribosome.Api.Effect"), reexported from [Ribosome.Api]("Ribosome.Api"), contains
-- the same set of API functions, but as callable 'Sem' functions that use the data declarations with 'sync'.
--
-- [Ribosome.Api]("Ribosome.Api") additionally contains many composite functions using the Neovim API.
--
-- The API also defines the data types 'Buffer', 'Window' and 'Tabpage', which are abstract types carrying an internal
-- identifier generated by Neovim.

-- $errors
-- Ribosome uses
-- [polysemy-resume](https://hackage.haskell.org/package/polysemy-resume-0.5.0.0/docs/Polysemy-Resume.html)
-- extensively, which is a concept for tracking errors across interpreters by attaching them to a wrapper effect.
--
-- In short, when an interpreter is written for the effect @'Rpc' !! 'RpcError'@ (which is a symbolic alias for
-- @'Resumable' 'RpcError' 'Rpc'), every use of the bare effect 'Rpc' must be converted at some point, with the
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
-- have to be converted to 'HandlerError' (which is expected by the request dispatcher and part of the 'Handler' stack),
-- and all bare effects like 'Rpc' have to be resumed or restopped since their interpreters only operate on the
-- 'Resumable' variants.
--
-- To make this chore a little less verbose, the class 'ToErrorMessage' can be leveraged to convert errors to
-- 'HandlerError', which consists of an 'ErrorMessage' and 'HandlerTag', which optionally identifies the plugin
-- component that threw the error.
--
-- Since 'RpcError' is an instance of 'ToErrorMessage', the combinators 'resumeHandlerError' and 'mapHandlerError' can
-- be used to reinterpret to @'Stop' 'HandlerError'@.
