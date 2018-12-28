module Ribosome.Internal.NvimObject(
  deriveString,
  extractObject,
  extractObjectOr,
) where

import qualified Data.ByteString.UTF8 as UTF8 (fromString)
import Data.Map.Strict (Map, (!?))
import Data.Text.Prettyprint.Doc ((<+>), pretty)
import Neovim (Object(ObjectString), Doc, AnsiStyle, fromObject, NvimObject)

deriveString :: (String -> a) -> Object -> Either (Doc AnsiStyle) a
deriveString cons o = fmap cons (fromObject o :: Either (Doc AnsiStyle) String)

objectKeyMissing :: String -> Maybe Object -> Either (Doc AnsiStyle) Object
objectKeyMissing _ (Just o) = Right o
objectKeyMissing key Nothing = Left (pretty "missing key in nvim data:" <+> pretty key)

extractObject :: NvimObject o => String -> Map Object Object -> Either (Doc AnsiStyle) o
extractObject key data' = do
  value <- objectKeyMissing key $ data' !? (ObjectString . UTF8.fromString) key
  fromObject value

extractObjectOr :: NvimObject o => String -> o -> Map Object Object -> o
extractObjectOr key default' data' =
  case extractObject key data' of
    Right a -> a
    Left _ -> default'
