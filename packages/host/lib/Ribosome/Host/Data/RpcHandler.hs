module Ribosome.Host.Data.RpcHandler where

import Data.MessagePack (Object)
import Exon (exon)
import Text.Show (showParen, showsPrec)

import Ribosome.Host.Data.Execution (Execution)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.Request (RpcMethod (RpcMethod))
import qualified Ribosome.Host.Data.RpcType as RpcType
import Ribosome.Host.Data.RpcType (RpcType)

type RpcHandlerFun r =
  [Object] -> Sem (Error HandlerError : r) Object

data RpcHandler r =
  RpcHandler {
    rpcType :: RpcType,
    name :: Text,
    execution :: Execution,
    handler :: RpcHandlerFun r
  }
  deriving stock (Generic)

instance Show (RpcHandler m) where
  showsPrec p (RpcHandler t n e _) =
    showParen (p > 10) [exon|RpcHandler #{showsPrec 11 t} #{showsPrec 11 n} #{showsPrec 11 e}|]

hoistRpcHandler ::
  (∀ x . Sem (Error HandlerError : r) x -> Sem (Error HandlerError : r1) x) ->
  RpcHandler r ->
  RpcHandler r1
hoistRpcHandler f RpcHandler {..} =
  RpcHandler {handler = f . handler, ..}

rpcMethod ::
  RpcType ->
  Text ->
  RpcMethod
rpcMethod rpcType name =
  RpcMethod [exon|#{RpcType.methodPrefix rpcType}:#{name}|]

rpcHandlerMethod :: RpcHandler r -> RpcMethod
rpcHandlerMethod RpcHandler {rpcType, name} =
  rpcMethod rpcType name
