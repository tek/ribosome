module Ribosome.Host.Data.RpcDef where

import Data.MessagePack (Object)
import Exon (exon)
import Text.Show (showParen, showsPrec)

import Ribosome.Host.Data.Execution (Execution)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.Request (RpcMethod (RpcMethod))
import qualified Ribosome.Host.Data.RpcType as RpcType
import Ribosome.Host.Data.RpcType (RpcType)

type RpcHandler r =
  [Object] -> Sem (Error HandlerError : r) Object

data RpcDef r =
  RpcDef {
    rpcType :: RpcType,
    name :: Text,
    execution :: Execution,
    handler :: RpcHandler r
  }

instance Show (RpcDef m) where
  showsPrec p (RpcDef t n e _) =
    showParen (p > 10) [exon|RpcDef #{showsPrec 11 t} #{showsPrec 11 n} #{showsPrec 11 e}|]

hoistRpcDef ::
  (∀ x . Sem (Error HandlerError : r) x -> Sem (Error HandlerError : r1) x) ->
  RpcDef r ->
  RpcDef r1
hoistRpcDef f RpcDef {..} =
  RpcDef {handler = f . handler, ..}

rpcMethod :: RpcDef r -> RpcMethod
rpcMethod RpcDef {rpcType, name} =
  RpcMethod [exon|#{RpcType.methodPrefix rpcType}:#{name}|]
