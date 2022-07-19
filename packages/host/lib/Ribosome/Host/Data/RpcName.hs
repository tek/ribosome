module Ribosome.Host.Data.RpcName where

newtype RpcName =
  RpcName { unRpcName :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)
