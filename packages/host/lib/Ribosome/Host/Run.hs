module Ribosome.Host.Run where

import Conc (ChanConsumer, ChanEvents, interpretAtomic, interpretEventsChan)
import Polysemy.Process (Process)

import Ribosome.Host.Data.Event (Event)
import Ribosome.Host.Data.HostConfig (LogConfig)
import Ribosome.Host.Data.Report (LogReport)
import Ribosome.Host.Data.Request (RequestId)
import Ribosome.Host.Data.Response (Response)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import Ribosome.Host.Effect.Reports (Reports)
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Effect.UserError (UserError)
import Ribosome.Host.IOStack (IOStack)
import Ribosome.Host.Interpreter.Log (interpretDataLogRpc, interpretLogRpc)
import Ribosome.Host.Interpreter.Reports (interpretReports)
import Ribosome.Host.Interpreter.Responses (interpretResponses)
import Ribosome.Host.Interpreter.Rpc (interpretRpc)

type RpcProcess =
  Process RpcMessage (Either Text RpcMessage)

type RpcStack =
  [
    Log,
    DataLog LogReport,
    Rpc !! RpcError,
    Responses RequestId Response !! RpcError,
    ChanEvents Event,
    ChanConsumer Event,
    Reports
  ]

type RpcDeps =
  [
    RpcProcess,
    UserError
  ]

interpretRpcStack ::
  Members IOStack r =>
  Members RpcDeps r =>
  Members [Log, Reader LogConfig] r =>
  InterpretersFor RpcStack r
interpretRpcStack =
  interpretReports .
  interpretEventsChan @Event .
  interpretResponses .
  interpretAtomic Nothing .
  interpretRpc .
  raiseUnder .
  interpretDataLogRpc .
  interpretLogRpc
