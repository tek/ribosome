{-# options_haddock prune #-}

module Ribosome.Host.Data.RpcHandler where

import Data.MessagePack (Object)
import Exon (exon)
import Text.Show (showParen, showsPrec)

import Ribosome.Host.Data.Execution (Execution)
import Ribosome.Host.Data.HandlerError (HandlerError, resumeHandlerError)
import Ribosome.Host.Data.Request (RpcMethod (RpcMethod))
import Ribosome.Host.Data.RpcError (RpcError)
import qualified Ribosome.Host.Data.RpcType as RpcType
import Ribosome.Host.Data.RpcType (RpcType)
import Ribosome.Host.Effect.Rpc (Rpc)

-- |A request handler function is a 'Sem' with arbitrary stack that has an error of type 'HandlerError' at its head.
--
-- These error messages are reported to the user by return value for synchronous requests and via @echo@ for
-- asynchronous ones, provided that the severity specified in the error is greater than the log level set in
-- 'Ribosome.UserError'.
--
-- If the plugin was started with @--log-file@, it is also written to the file log.
-- Additionally, errors are stored in memory by the effect 'Ribosome.Errors'.
type Handler r a =
  Sem (Stop HandlerError : r) a

type RpcHandlerFun r =
  [Object] -> Handler r Object

-- |This type defines a request handler, using a 'Handler' function, the request type, a name, and whether it should
-- block Neovim while executing.
-- It can be constructed from functions using 'Ribosome.rpcFunction', 'Ribosome.rpcCommand' and 'Ribosome.rpcAutocmd'.
--
-- A list of 'RpcHandler's can be used as a Neovim plugin by passing them to 'Ribosome.runNvimHandlersIO'.
--
-- For an explanation of 'Stop', see [Errors]("Ribosome#errors").
data RpcHandler r =
  RpcHandler {
    -- |Whether the trigger is a function, command, or autocmd, and the various options Neovim offers for them.
    rpcType :: RpcType,
    -- |An identifier used to associate a request with a handler, which is also used as the name  of the function or
    -- command.
    rpcName :: Text,
    -- |If this is 'Ribosome.Sync', the handler will block Neovim via @rpcrequest@.
    -- If it is 'Ribosome.Async', Neovim will send @rpcnotify@ and forget about it.
    rpcExecution :: Execution,
    -- |The function operating on raw msgpack objects, derived from a 'Handler' by the smart constructors.
    rpcHandler :: RpcHandlerFun r
  }
  deriving stock (Generic)

instance Show (RpcHandler m) where
  showsPrec p (RpcHandler t n e _) =
    showParen (p > 10) [exon|RpcHandler #{showsPrec 11 t} #{showsPrec 11 n} #{showsPrec 11 e}|]

hoistRpcHandler ::
  (∀ x . Sem (Stop HandlerError : r) x -> Sem (Stop HandlerError : r1) x) ->
  RpcHandler r ->
  RpcHandler r1
hoistRpcHandler f RpcHandler {..} =
  RpcHandler {rpcHandler = f . rpcHandler, ..}

hoistRpcHandlers ::
  (∀ x . Sem (Stop HandlerError : r) x -> Sem (Stop HandlerError : r1) x) ->
  [RpcHandler r] ->
  [RpcHandler r1]
hoistRpcHandlers f =
  fmap (hoistRpcHandler f)

rpcMethod ::
  RpcType ->
  Text ->
  RpcMethod
rpcMethod rpcType name =
  RpcMethod [exon|#{RpcType.methodPrefix rpcType}:#{name}|]

rpcHandlerMethod :: RpcHandler r -> RpcMethod
rpcHandlerMethod RpcHandler {rpcType, rpcName} =
  rpcMethod rpcType rpcName

simpleHandler ::
  Member (Rpc !! RpcError) r =>
  Sem (Rpc : Stop HandlerError : r) a ->
  Handler r a
simpleHandler =
  resumeHandlerError
