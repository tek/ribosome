-- |Interpreters for basic plugin effects down to 'IO'
module Ribosome.IOStack where

import Ribosome.Cli (withCli)
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Host.IOStack (BasicStack, runBasicStack)

-- |The effects that are shared by all variants (like embedded, remote, socket) of main functions.
--
-- Contains logging effects, IO related stuff and the plugin's name in a 'Reader'.
type BasicPluginStack =
  Reader PluginName : BasicStack

-- |Execute the basic plugin stack all the way to an 'IO', using 'PluginConfig' for the name and logging settings.
runBasicPluginStack ::
  PluginConfig ->
  Sem BasicPluginStack () ->
  IO ()
runBasicPluginStack (PluginConfig name conf) =
  runBasicStack conf .
  runReader name

-- |Execute the basic plugin stack all the way to an 'IO' like 'runBasicPluginStack', reading config overrides from
-- command line options.
runCli ::
  PluginConfig ->
  Sem BasicPluginStack () ->
  IO ()
runCli defaultConf prog =
  withCli defaultConf \ conf ->
    runBasicPluginStack conf prog
