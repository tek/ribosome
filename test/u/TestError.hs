{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module TestError where

import Data.DeepPrisms (deepPrisms)

import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Data.Mapping (MappingError)
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.RpcCall (RpcError)

data TestError =
  Rpc RpcError
  |
  Decode DecodeError
  |
  Mapping MappingError
  deriving (Generic, ReportError)

deepPrisms ''TestError

handleTestError :: TestError -> Ribo s TestError ()
handleTestError _ =
  return ()

type RiboT a = Ribo () TestError a
