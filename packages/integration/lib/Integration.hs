module Integration where

import Data.MessagePack (Object (ObjectInt))
import Log (Severity (Debug))
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.HostConfig (setStderr)
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler))
import qualified Ribosome.Host.Data.RpcType as RpcType
import Ribosome.Remote (runNvimHandlersIO)

hand ::
  [Object] ->
  Sem r Object
hand _ =
  pure (ObjectInt 5)

handlers ::
  [RpcHandler r]
handlers =
  [RpcHandler RpcType.Function "Test" Sync hand]

integrationTest :: IO ()
integrationTest =
  runNvimHandlersIO (PluginConfig "int" (setStderr Debug def)) handlers
