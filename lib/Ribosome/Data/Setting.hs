{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ribosome.Data.Setting where

import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import System.Log (Priority(NOTICE))

import Ribosome.Data.DeepError (deepError)
import Ribosome.Data.ErrorReport (ErrorReport(..))
import Ribosome.Error.Report1 (ReportError(..))
import Ribosome.Nvim.Api.RpcCall (AsRpcError(_RpcError), RpcError, _Nvim)

data SettingError =
  Rpc RpcError
  |
  Decode String (Doc AnsiStyle)
  |
  Unset String
  deriving Show

deepError ''SettingError

instance ReportError SettingError where
  errorReport (Rpc err) =
    errorReport err
  errorReport (Decode name message) =
    ErrorReport ("invalid setting: " ++ name) ["failed to decode setting `" ++ name ++ "`", show message] NOTICE
  errorReport (Unset name) =
    ErrorReport ("required setting unset: " ++ name) ["unset setting: `" ++ name ++ "`"] NOTICE

data Setting a =
  Setting {
    name :: String,
    prefix :: Bool,
    fallback :: Maybe a
  }
