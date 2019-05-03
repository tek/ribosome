{-# LANGUAGE TemplateHaskell #-}

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
  deriving Show

deepPrisms ''TestError

instance ReportError TestError where
  errorReport (Rpc e) = errorReport e
  errorReport (Decode e) = errorReport e
  errorReport (Mapping e) = errorReport e

handleTestError :: TestError -> Ribo s TestError ()
handleTestError _ =
  return ()

type RiboT a = Ribo () TestError a
