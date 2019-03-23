{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Data.SettingError where

import Data.DeepPrisms (deepPrisms)
import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import System.Log (Priority(NOTICE))

import Ribosome.Data.ErrorReport (ErrorReport(..))
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Nvim.Api.RpcCall (RpcError)

data SettingError =
  Rpc RpcError
  |
  Decode String (Doc AnsiStyle)
  |
  Unset String
  deriving Show

deepPrisms ''SettingError

instance ReportError SettingError where
  errorReport (Rpc err) =
    errorReport err
  errorReport (Decode name message) =
    ErrorReport ("invalid setting: " ++ name) ["failed to decode setting `" ++ name ++ "`", show message] NOTICE
  errorReport (Unset name) =
    ErrorReport ("required setting unset: " ++ name) ["unset setting: `" ++ name ++ "`"] NOTICE
