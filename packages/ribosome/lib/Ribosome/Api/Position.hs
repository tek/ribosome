-- |API functions for @getpos@.
module Ribosome.Api.Position where

import Ribosome.Host.Api.Data (nvimCallFunction)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.RpcCall (RpcCall)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)

-- |'RpcCall' for the function @getpos@ that returns a 4-tuple.
getposCall ::
  Text ->
  RpcCall (Int, Int, Int, Int)
getposCall expr =
  nvimCallFunction "getpos" [toMsgpack expr]

-- |Call the function @getpos@ and return a 4-tuple.
getpos ::
  Member Rpc r =>
  Text ->
  Sem r (Int, Int, Int, Int)
getpos =
  Rpc.sync . getposCall

-- |Return the start and end coordinates of visual mode.
visualPos ::
  Member Rpc r =>
  Sem r ((Int, Int), (Int, Int))
visualPos = do
  ((_, lnumStart, colStart, _), (_, lnumEnd, colEnd, _)) <- Rpc.sync do
    start <- getposCall "'<"
    end <- getposCall "'>"
    pure (start, end)
  pure ((lnumStart, colStart), (lnumEnd, colEnd))
