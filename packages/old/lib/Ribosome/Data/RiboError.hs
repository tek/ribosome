module Ribosome.Data.RiboError where

import Ribosome.Data.Mapping (MappingError)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Error.Report.Class (ReportError (..))
import Ribosome.Msgpack.Error (DecodeError)

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
  deriving stock (Show, Generic)
  deriving anyclass (ReportError)

deepPrisms ''RiboError
