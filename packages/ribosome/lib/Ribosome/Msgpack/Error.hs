module Ribosome.Msgpack.Error where

import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import System.Log.Logger (Priority (ERROR))

import Ribosome.Data.ErrorReport (ErrorReport (ErrorReport))
import Ribosome.Error.Report.Class (ReportError (..))
import Ribosome.Msgpack.Util (Err)

newtype DecodeError =
  Failed Err
  deriving stock (Show)

deepPrisms ''DecodeError

instance ReportError DecodeError where
  errorReport (Failed err) =
    ErrorReport "error decoding response from neovim" ["DecodeError:", rendered] ERROR
    where
      rendered = renderStrict $ layoutPretty defaultLayoutOptions err
