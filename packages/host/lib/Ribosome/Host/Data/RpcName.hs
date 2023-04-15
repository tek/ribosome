-- |The name of an RPC handler
module Ribosome.Host.Data.RpcName where

-- |This name is used for the function or command registered in Neovim as well as to internally identify a handler.
newtype RpcName =
  RpcName { unRpcName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)
