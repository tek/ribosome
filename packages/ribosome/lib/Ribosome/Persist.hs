module Ribosome.Persist where

-- safeDecodeFile ::
--   MonadIO m =>
--   FromJSON a =>
--   Path Abs File ->
--   m a
-- safeDecodeFile file = do
--   result <- either (fileNotReadable file) pure =<< (liftIO . try . eitherDecodeFileStrict' . toFilePath $ file)
--   either (decodeError file) pure . mapLeft toText $ result
