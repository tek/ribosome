module Ribosome.Api.Position where

import Ribosome.Host.Api.Data (nvimCallFunction)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.RpcCall (RpcCall)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)

getposCall ::
  Text ->
  RpcCall (Int, Int, Int, Int)
getposCall expr =
  nvimCallFunction "getpos" [toMsgpack expr]

getpos ::
  Member Rpc r =>
  Text ->
  Sem r (Int, Int, Int, Int)
getpos =
  Rpc.sync . getposCall

visualPos ::
  Member Rpc r =>
  Sem r ((Int, Int), (Int, Int))
visualPos = do
  ((_, lnumStart, colStart, _), (_, lnumEnd, colEnd, _)) <- Rpc.sync do
    start <- getposCall "'<"
    end <- getposCall "'>"
    pure (start, end)
  pure ((lnumStart, colStart), (lnumEnd, colEnd))
