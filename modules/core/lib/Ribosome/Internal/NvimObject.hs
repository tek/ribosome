module Ribosome.Internal.NvimObject where

import Data.Map (Map, (!?))
import Data.Text.Prettyprint.Doc (pretty, (<+>))
import Neovim (AnsiStyle, Doc, NvimObject, Object(ObjectString), fromObject)

deriveString :: (Text -> a) -> Object -> Either (Doc AnsiStyle) a
deriveString cons o = fmap cons (fromObject o :: Either (Doc AnsiStyle) Text)

objectKeyMissing :: Text -> Maybe Object -> Either (Doc AnsiStyle) Object
objectKeyMissing _ (Just o) = Right o
objectKeyMissing key Nothing = Left (pretty ("missing key in nvim data:" :: Text) <+> pretty key)

extractObject :: NvimObject o => Text -> Map Object Object -> Either (Doc AnsiStyle) o
extractObject key data' = do
  value <- objectKeyMissing key $ data' !? (ObjectString . encodeUtf8) key
  fromObject value

extractObjectOr :: NvimObject o => Text -> o -> Map Object Object -> o
extractObjectOr key default' data' =
  case extractObject key data' of
    Right a -> a
    Left _ -> default'
