module Ribosome.IOStack where

import Ribosome.Cli (withCli)
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Host.IOStack (BasicStack, runBasicStack)

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
