module TestError where

import Hedgehog (TestT)
import Ribosome.Data.Mapping (MappingError)
import Ribosome.Error.Report.Class (ReportError)
import Ribosome.Msgpack.Error (DecodeError)

data TestError =
  Rpc RpcError
  |
  Decode DecodeError
  |
  Mapping MappingError
  deriving stock (Show, Generic)
  deriving anyclass (ReportError)

deepPrisms ''TestError

handleTestError :: TestError -> Ribo s TestError ()
handleTestError =
  showError "error in test:"

type RiboTest a = TestT (Ribo () TestError) a
