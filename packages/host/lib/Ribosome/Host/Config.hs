module Ribosome.Host.Config where

import qualified Ribosome.Host.Data.HostConfig as HostConfig
import Ribosome.Host.Data.HostConfig (HostConfig (HostConfig), LogConfig)

interpretLogConfig ::
  Member (Reader HostConfig) r =>
  InterpreterFor (Reader LogConfig) r
interpretLogConfig sem =
  ask >>= \ HostConfig {hostLog} ->
    runReader hostLog sem
