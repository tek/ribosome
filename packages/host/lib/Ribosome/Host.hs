module Ribosome.Host (
  module Ribosome.Host.Class.Msgpack.Array,
  module Ribosome.Host.Class.Msgpack.Decode,
  module Ribosome.Host.Class.Msgpack.Encode,
  module Ribosome.Host.Class.Msgpack.Map,
  module Ribosome.Host.Data.Execution,
  module Ribosome.Host.Data.HandlerError,
  module Ribosome.Host.Data.HostConfig,
  module Ribosome.Host.Data.RpcError,
  module Ribosome.Host.Data.RpcHandler,
  module Ribosome.Host.Data.StoredError,
  module Ribosome.Host.Effect.Errors,
  module Ribosome.Host.Effect.Rpc,
  module Ribosome.Host.Embed,
  module Ribosome.Host.Handler,
  module Ribosome.Host.Remote,
) where

import Prelude hiding (async)

import Ribosome.Host.Class.Msgpack.Array (msgpackArray)
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Host.Class.Msgpack.Map (msgpackMap)
import Ribosome.Host.Data.Execution (Execution (Async, Sync))
import Ribosome.Host.Data.HandlerError (
  ErrorMessage (ErrorMessage),
  HandlerError (HandlerError),
  HandlerTag (..),
  ToErrorMessage (toErrorMessage),
  mapHandlerError,
  resumeHandlerError,
  resumeHandlerErrorFrom,
  toHandlerError,
  )
import Ribosome.Host.Data.HostConfig (HostConfig (..), LogConfig (..), setStderr)
import Ribosome.Host.Data.RpcError (RpcError (RpcError))
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler (RpcHandler))
import Ribosome.Host.Data.StoredError (StoredError (StoredError))
import Ribosome.Host.Effect.Errors (Errors)
import Ribosome.Host.Effect.Rpc (Rpc, async, notify, sync)
import Ribosome.Host.Embed (embedNvim, embedNvimConf, embedNvim_, interpretHostEmbed, withHostEmbed)
import Ribosome.Host.Handler (rpcAutocmd, rpcCommand, rpcFunction)
import Ribosome.Host.Remote (interpretHostRemote, runNvimHost, runNvimHostHandlersIO, runNvimHostIO)
