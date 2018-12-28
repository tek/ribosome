module Ribosome.Api.Response(
  nvimFatal,
  nvimResponseString,
  nvimResponseStringArray,
  nvimValidateFatal,
)
where

import Data.ByteString.UTF8
import Neovim

nvimFatal :: Either NeovimException a -> Neovim env a
nvimFatal (Right a) = return a
nvimFatal (Left e) = (liftIO . fail . show) e

nvimResponseString :: Object -> Neovim env String
nvimResponseString (ObjectString a) = return $ toString a
nvimResponseString a = liftIO . fail $ "invalid nvim type for String: " ++ show a

nvimResponseStringArray :: Object -> Neovim env [String]
nvimResponseStringArray (ObjectArray a) = traverse nvimResponseString a
nvimResponseStringArray a = liftIO . fail $ "invalid nvim type for Array: " ++ show a

nvimValidateFatal :: (Object -> Neovim env a) -> Either NeovimException Object -> Neovim env a
nvimValidateFatal validate response = do
  result <- nvimFatal response
  validate result
