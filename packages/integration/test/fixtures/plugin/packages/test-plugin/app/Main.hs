module Main where

import Data.MessagePack (Object (ObjectInt))
import qualified Data.Text.IO as Text
import Exon (exon)
import qualified Polysemy.Log as Log
import Polysemy.Log (Severity (Warn), interpretLogStderrLevelConc)
import Polysemy.Process.Data.ProcessError (ProcessError)
import Polysemy.Time (interpretTimeGhc)
import Ribosome.Host.Api.Effect (nvimEcho)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.BootError (unBootError)
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.RpcError (RpcError (RpcError))
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler))
import qualified Ribosome.Host.Data.RpcType as RpcType
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpreter.UserError (interpretUserErrorInfo)
import Ribosome.Host.Remote (runNvimPlugin)
import System.IO (stderr)

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

errorStderr :: IO (Either Text ()) -> IO ()
errorStderr ma =
  ma >>= \case
    Left err -> Text.hPutStrLn stderr err
    Right () -> unit

main :: IO ()
main =
  errorStderr $
  runConc $
  runError $
  mapError unBootError $
  mapError @ProcessError show $
  interpretTimeGhc $
  interpretLogStderrLevelConc (Just Warn) $
  interpretUserErrorInfo $
  embed (Text.writeFile "/home/tek/test-log" "start") *> Log.error "starting" *> runNvimPlugin handlers
