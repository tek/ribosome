{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Nvim.Api.RpcCall where

import Data.Either.Combinators (mapLeft)
import Data.MessagePack (Object)
import Neovim (Neovim)
import Neovim.Exceptions (NeovimException)
import Neovim.Plugin.Classes (FunctionName)
import Neovim.RPC.FunctionCall (scall)

import Ribosome.Data.DeepPrisms (deepPrisms)
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Msgpack.Decode (MsgpackDecode(..))
import Ribosome.Msgpack.Util (Err)

data RpcCall =
  RpcCall FunctionName [Object]
  deriving (Eq, Show)

newtype AsyncRpcCall =
  AsyncRpcCall RpcCall
  deriving (Eq, Show)

newtype SyncRpcCall =
  SyncRpcCall RpcCall
  deriving (Eq, Show)

data RpcError =
  Decode Err
  |
  Nvim NeovimException
  deriving Show

deepPrisms ''RpcError

class Rpc c a where
  call :: c -> Neovim e (Either RpcError a)

instance Rpc AsyncRpcCall () where
  call (AsyncRpcCall (RpcCall name args)) =
    mapLeft Nvim <$> scall name args

instance MsgpackDecode a => Rpc SyncRpcCall a where
  call (SyncRpcCall (RpcCall name args)) =
    either (Left . Nvim) (mapLeft Decode . fromMsgpack) <$> scall name args

instance ReportError RpcError where
  errorReport (Decode _) =
    undefined
  errorReport (Nvim _)  =
    undefined
