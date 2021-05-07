# About

*ribosome* is an extension framework for [nvim-hs], a [Haskell] library that
provides a [Neovim] plugin engine.

*ribosome*'s structure is similar to that of vanilla [nvim-hs] plugins and is
intended to provide a more comfortable API on top of it.

# Basic Plugin

*ribosome* comes with a companion plugin manager, [chromatin], but you can
start plugins regularly with `jobstart()`.

First, add `ribosome` to your `myplugin.cabal`'s dependencies.

Your project should have an executable that looks like this:

```haskell
import Neovim (neovim, plugins, defaultConfig)
import MyPlugin.Plugin (plugin)

main :: IO ()
main =
  neovim defaultConfig {plugins = [plugin]}
```

In `MyPlugin.Plugin`, you should write a plugin definition like this:

```haskell
module MyPlugin.Plugin where

import Data.Default.Class (Default(def))
import Neovim (Neovim, NeovimPlugin, Plugin, wrapPlugin)
import Neovim.Context.Internal (Config(customConfig), asks')
import Ribosome.Api.Echo (echom)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, Ribo)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Error.Report (reportError)
import Ribosome.Internal.IO (retypeNeovim)
import Ribosome.Plugin (RpcDef, autocmd, cmd, riboPlugin, rpcHandler)
import System.Log.Logger (Priority(ERROR), setLevel, updateGlobalLogger)

handleError :: Text -> Ribo () Text ()
handleError err =
  echom err

hello :: NvimE e m => MonadRibo m => m ()
hello ::
  echom "hello"

bufEnter :: NvimE e m => MonadRibo m => m ()
bufEnter ::
  echom "BufEnter"

rpcHandlers :: [[RpcDef (Ribo () Error)]]
rpcHandlers =
  [
    $(rpcHandler (cmd []) 'hello),
    $(rpcHandler (autocmd "BufEnter") 'bufEnter)
  ]

plugin' :: Ribosome () -> Plugin (Ribosome ())
plugin' env =
  riboPlugin "myplugin" env rpcHandlers def handleError def

initialize :: Neovim e (Ribosome Env)
initialize = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  ribo <- newRibosome "myplugin" def
  retypeNeovim (const ribo) (asks' customConfig)

plugin :: Neovim e NeovimPlugin
plugin =
  wrapPlugin . plugin' =<< initialize
```

Follow the instructions for the bootstrapping vim config in the documentation
for [chromatin].
Now you can execute the command `:Hello`.

For more inspiration, check out some example projects: [proteome], [myo],
[uracil].

[nvim-hs]: https://github.com/neovimhaskell/nvim-hs
[Haskell]: https://www.haskell.org
[proteome]: https://github.com/tek/proteome
[myo]: https://github.com/tek/myo
[uracil]: https://github.com/tek/uracil
[chromatin]: https://github.com/tek/chromatin
[nvim-hs]: https://github.com/neovimhaskell/nvim-hs
[Neovim]: https://github.com/neovim/neovim
