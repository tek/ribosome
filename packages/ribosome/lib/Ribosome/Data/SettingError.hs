module Ribosome.Data.SettingError where

newtype SettingError =
  SettingError { unSettingError :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

-- instance ReportError SettingError where
--   errorReport (Decode name message) =
--     ErrorReport ("invalid setting: " <> name) ["failed to decode setting `" <> name <> "`", show message] NOTICE
--   errorReport (Unset name) =
--     ErrorReport ("required setting unset: " <> name) ["unset setting: `" <> name <> "`"] NOTICE
