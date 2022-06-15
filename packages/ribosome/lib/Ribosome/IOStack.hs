module Ribosome.IOStack where

import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.IOStack (BasicStack, runBasicStack)
import Ribosome.Cli (withCli)

type BasicPluginStack =
  Reader PluginName : BasicStack

runBasicPluginStack ::
  PluginConfig ->
  Sem BasicPluginStack () ->
  IO ()
runBasicPluginStack (PluginConfig name conf) =
  runBasicStack conf .
  runReader name

runCli ::
  PluginConfig ->
  Sem BasicPluginStack () ->
  IO ()
runCli defaultConf prog =
  withCli defaultConf \ conf ->
    runBasicPluginStack conf prog

type TestEffects =
  [
    Scratch,
    Settings,
    Rpc
  ]
