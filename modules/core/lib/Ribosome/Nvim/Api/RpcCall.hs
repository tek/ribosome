{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Nvim.Api.RpcCall where

import Data.DeepPrisms (deepPrisms)
import Data.Either.Combinators (mapLeft)
import Data.MessagePack (Object)
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Neovim (Neovim)
import Neovim.Exceptions (NeovimException)
import Neovim.Plugin.Classes (FunctionName)
import Neovim.RPC.FunctionCall (scall)
import System.Log.Logger (Priority(ERROR))

import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Msgpack.Decode (MsgpackDecode(..))
import Ribosome.Msgpack.Util (Err)

data RpcCall =
  RpcCall {
    rpcCallName :: FunctionName,
    rpcCallArgs :: [Object]
  }
  deriving (Eq, Show)

newtype AsyncRpcCall =
  AsyncRpcCall { asyncRpcCall :: RpcCall }
  deriving (Eq, Show)

newtype SyncRpcCall =
  SyncRpcCall { syncRpcCall :: RpcCall }
  deriving (Eq, Show)

data RpcError =
  Decode Err
  |
  Nvim RpcCall NeovimException
  |
  Atomic Text
  deriving Show

deepPrisms ''RpcError

class Rpc c a where
  call :: c -> Neovim e (Either RpcError a)

instance Rpc AsyncRpcCall () where
  call (AsyncRpcCall c@(RpcCall name args)) =
    mapLeft (Nvim c) <$> scall name args

instance MsgpackDecode a => Rpc SyncRpcCall a where
  call (SyncRpcCall c@(RpcCall name args)) =
    either (Left . Nvim c) (mapLeft Decode . fromMsgpack) <$> scall name args

instance ReportError RpcError where
  errorReport (Decode err) =
    ErrorReport "error decoding neovim response" ["RpcError.Decode:", rendered] ERROR
    where
      rendered = renderStrict $ layoutPretty defaultLayoutOptions err
  errorReport (Nvim c exc)  =
    ErrorReport "error in request to neovim" ["RpcError.Nvim:", show c, show exc] ERROR
  errorReport (Atomic msg)  =
    ErrorReport "error in request to neovim" ["RpcError.Atomic:", msg] ERROR
