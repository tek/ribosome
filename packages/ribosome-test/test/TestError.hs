module TestError where

import Hedgehog (TestT)
import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Data.Mapping (MappingError)
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Log (showError)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.RpcCall (RpcError)

data TestError =
  Rpc RpcError
  |
  Decode DecodeError
  |
  Mapping MappingError
  deriving (Show, Generic, ReportError)

deepPrisms ''TestError

handleTestError :: TestError -> Ribo s TestError ()
handleTestError =
  showError "error in test:"

type RiboTest a = TestT (Ribo () TestError) a
