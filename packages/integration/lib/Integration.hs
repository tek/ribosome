module Integration where

import Data.MessagePack (Object (ObjectInt))
import Exon (exon)
import Log (Severity (Debug))
import qualified Polysemy.Log as Log
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Host.Api.Effect (nvimEcho)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.HostConfig (log, logLevelStderr)
import Ribosome.Host.Data.RpcError (RpcError (RpcError))
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler))
import qualified Ribosome.Host.Data.RpcType as RpcType
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Remote (runNvimPluginIO_)

hand ::
  Members [Rpc !! RpcError, Log] r =>
  [Object] ->
  Sem r Object
hand _ = do
  nvimEcho [toMsgpack (["hello"] :: [Text])] False mempty !! \ (RpcError e) ->
    Log.error [exon|hand: #{e}|]
  pure (ObjectInt 5)

handlers ::
  Members [Rpc !! RpcError, Log] r =>
  [RpcHandler r]
handlers =
  [
    RpcHandler RpcType.Function "Test" Sync hand
  ]

integrationTest :: IO ()
integrationTest =
  runNvimPluginIO_ (PluginConfig "int" def { log = def { logLevelStderr = Debug } }) mempty mempty handlers
