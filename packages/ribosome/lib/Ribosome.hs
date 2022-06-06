module Ribosome (
  -- * Introduction

  -- $intro

  -- * Creating a project

  -- $project

  module Ribosome.Effect.Scratch,
  module Ribosome.Effect.Settings,
  module Ribosome.Data.FloatOptions,
  module Ribosome.Data.PluginConfig,
  module Ribosome.Data.Register,
  module Ribosome.Data.RegisterType,
  module Ribosome.Data.ScratchId,
  module Ribosome.Data.ScratchOptions,
  module Ribosome.Data.ScratchState,
  module Ribosome.Data.Setting,
  module Ribosome.Data.SettingError,
  module Ribosome.Embed,
  module Ribosome.Host,
  module Ribosome.IOStack,
  module Ribosome.Locks,
  module Ribosome.Remote,
) where

import Ribosome.Data.FloatOptions (FloatOptions (FloatOptions))
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig), pluginNamed)
import Ribosome.Data.Register (Register, registerRepr)
import Ribosome.Data.RegisterType (RegisterType)
import Ribosome.Data.ScratchId (ScratchId (ScratchId))
import Ribosome.Data.ScratchOptions (ScratchOptions (ScratchOptions), defaultScratchOptions)
import Ribosome.Data.ScratchState (ScratchState (ScratchState))
import Ribosome.Data.Setting (Setting (Setting))
import Ribosome.Data.SettingError (SettingError (SettingError))
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Embed (embedNvimPlugin, embedNvimPlugin_, interpretPluginEmbed, testPluginEmbed)
import Ribosome.Host
import Ribosome.IOStack (BasicPluginStack, TestEffects, runBasicPluginStack)
import Ribosome.Locks (lockOrSkip)
import Ribosome.Remote (
  RemoteStack,
  interpretPluginRemote,
  runNvimHandlers,
  runNvimHandlersIO,
  runNvimHandlersIO_,
  runNvimHandlers_,
  runNvimPlugin,
  runNvimPluginIO,
  runNvimPluginIO_,
  runNvimPlugin_,
  runPluginHostRemote,
  )

-- $intro
-- This library provides a framework for building [Neovim](https://neovim.io) plugins with
-- [Polysemy](https://hackage.haskell.org/package/polysemy).
--
-- A plugin consists of a set of request handlers that can be executed by Neovim functions, commands, autocmds, or
-- events, and may communicate with Neovim by calling its RPC API.
--
-- Here is an example for a simple plugin with a single request handler.
--
-- Note that Ribosome uses the Polysemy prelude "Incipit", which needs to be imported if the plugin project doesn't
-- depend on it.
--
-- > -- import Incipit
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
-- The most reliable way to set up a repository for a plugin is to use the Nix build tool, for which Ribosome provides a
-- flake template that creates a skeleton project when executed:
--
-- > $ nix flake new my-plugin -t github:tek/ribosome
--
-- The created directory will contain a ready-to-use Neovim plugin that can be started like any other.
-- For example, moving it to @~/.local/share/nvim/site/pack/foo/opt/my-plugin@ will allow you to run:
--
-- > :packadd my-plugin
--
-- This will cause the plugin to be built, which may take a pretty long time if its dependencies are not in a Nix cache.
-- Once that is done, the template's dummy handler can be executed:
--
-- > :echo PluginPing()
-- > 0
-- > :echo PluginPing()
-- > 1
--
-- After the first build, the executable will be run directly, without checking for updates, unless the result has been
-- garbage collected by Nix (i.e. the @result@ link in the repo is broken).
-- In order to force a rebuild after pulling, run the command:
--
-- > $ nix build
