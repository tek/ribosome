module Ribosome.Host.Run where

import Conc (ChanConsumer, ChanEvents, interpretEventsChan)
import Polysemy.Process (Process)

import Ribosome.Host.Data.Event (Event)
import Ribosome.Host.Data.HostError (HostError)
import Ribosome.Host.Data.Request (RequestId)
import Ribosome.Host.Data.Response (Response)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import Ribosome.Host.Effect.Errors (Errors)
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Effect.UserError (UserError)
import Ribosome.Host.IOStack (IOStack)
import Ribosome.Host.Interpreter.Errors (interpretErrors)
import Ribosome.Host.Interpreter.Log (interpretDataLogRpc)
import Ribosome.Host.Interpreter.Responses (interpretResponses)
import Ribosome.Host.Interpreter.Rpc (interpretRpc)

type RpcProcess =
  Process RpcMessage (Either Text RpcMessage)

type RpcStack =
  [
    DataLog HostError,
    Rpc !! RpcError,
    Responses RequestId Response !! RpcError,
    ChanEvents Event,
    ChanConsumer Event,
    Errors
  ]

type RpcDeps =
  [
    RpcProcess,
    UserError
  ]

interpretRpcStack ::
  Members IOStack r =>
  Members RpcDeps r =>
  Member Log r =>
  InterpretersFor RpcStack r
interpretRpcStack =
  interpretErrors .
  interpretEventsChan @Event .
  interpretResponses .
  interpretRpc .
  interpretDataLogRpc
