-- |Interpreters for basic plugin effects down to 'IO'
module Ribosome.IOStack where

import Ribosome.Cli (withCli)
import Ribosome.Data.CustomConfig (CustomConfig (CustomConfig))
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Host.Data.HostConfig (HostConfig)
import Ribosome.Host.IOStack (BasicStack, runBasicStack)

-- |The effects that are shared by all variants (like embedded, remote, socket) of main functions.
--
-- Contains logging effects, IO related stuff and the plugin's name in a 'Reader'.
type BasicPluginStack c =
  Reader PluginName : Reader (CustomConfig c) : BasicStack

-- |Execute the basic plugin stack all the way to an 'IO', given the plugin name and logging settings.
runBasicPluginStack ::
  PluginName ->
  HostConfig ->
  c ->
  Sem (BasicPluginStack c) () ->
  IO ()
runBasicPluginStack name conf custom =
  runBasicStack conf .
  runReader (CustomConfig custom) .
  runReader name

-- |Execute the basic plugin stack all the way to an 'IO' like 'runBasicPluginStack', reading config overrides from
-- command line options.
runCli ::
  PluginConfig c ->
  Sem (BasicPluginStack c) () ->
  IO ()
runCli (PluginConfig name defaultConf customParser) prog =
  withCli name defaultConf customParser \ conf custom ->
    runBasicPluginStack name conf custom prog
