module Ribosome.Data.PluginName where

newtype PluginName =
  PluginName { unPluginName :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)
