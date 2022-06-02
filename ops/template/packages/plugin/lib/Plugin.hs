module Plugin where

import Conc (interpretAtomic, withAsync_)
import Ribosome (
  Execution (Sync),
  Handler,
  PluginConfig (PluginConfig),
  RpcHandler,
  rpcFunction,
  runNvimPluginIO,
  )

type PluginStack =
  '[
    AtomicState Int
  ]

conf :: PluginConfig
conf =
  PluginConfig "plugin" def

ping ::
  Member (AtomicState Int) r =>
  Handler r Int
ping =
  atomicState' \ s -> (s + 1, s + 1)

handlers ::
  Member (AtomicState Int) r =>
  [RpcHandler r]
handlers =
  [
    rpcFunction "PluginPing" Sync ping
  ]

prepare ::
  Sem r ()
prepare =
  unit

interpretPluginStack ::
  Members [Resource, Race, Async, Embed IO] r =>
  InterpretersFor PluginStack r
interpretPluginStack sem =
  interpretAtomic 0 do
    withAsync_ prepare sem

plugin :: IO ()
plugin =
  runNvimPluginIO @PluginStack conf mempty mempty handlers interpretPluginStack
