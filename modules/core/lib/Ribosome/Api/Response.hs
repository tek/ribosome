module Ribosome.Api.Response(
  nvimFatal,
  nvimResponseString,
  nvimResponseStringArray,
  nvimValidateFatal,
)
where

import Neovim

nvimFatal :: Either NeovimException a -> Neovim env a
nvimFatal (Right a) = return a
nvimFatal (Left e) = (liftIO . fail . show) e

nvimResponseString :: Object -> Neovim env Text
nvimResponseString (ObjectString a) = return $ decodeUtf8 a
nvimResponseString a = liftIO . fail $ "invalid nvim type for Text: " <> show a

nvimResponseStringArray :: Object -> Neovim env [Text]
nvimResponseStringArray (ObjectArray a) = traverse nvimResponseString a
nvimResponseStringArray a = liftIO . fail $ "invalid nvim type for Array: " <> show a

nvimValidateFatal :: (Object -> Neovim env a) -> Either NeovimException Object -> Neovim env a
nvimValidateFatal validate response = do
  result <- nvimFatal response
  validate result
