{-# LANGUAGE DeriveAnyClass #-}

module Ribosome.Data.RiboError where

import Ribosome.Data.Mapping (MappingError)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.RpcCall (RpcError)

data RiboError =
  Mapping MappingError
  |
  Decode DecodeError
  |
  Rpc RpcError
  |
  Persist PersistError
  |
  Setting SettingError
  deriving (Show, Generic, ReportError)

deepPrisms ''RiboError
