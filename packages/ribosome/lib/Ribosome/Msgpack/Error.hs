module Ribosome.Msgpack.Error where

import System.Log.Logger (Priority(ERROR))

import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Msgpack.Util (Err)

newtype DecodeError =
  Failed Err
  deriving Show

deepPrisms ''DecodeError

instance ReportError DecodeError where
  errorReport (Failed err) =
    ErrorReport "error decoding response from neovim" ["DecodeError:", rendered] ERROR
    where
      rendered = renderStrict $ layoutPretty defaultLayoutOptions err
