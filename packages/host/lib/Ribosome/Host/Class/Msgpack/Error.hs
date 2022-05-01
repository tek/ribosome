module Ribosome.Host.Class.Msgpack.Error where

newtype DecodeError =
  DecodeError { unDecodeError :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)

-- instance ReportError DecodeError where
--   errorReport (Failed err) =
--     ErrorReport "error decoding response from neovim" ["DecodeError:", rendered] ERROR
--     where
--       rendered = renderStrict $ layoutPretty defaultLayoutOptions err
