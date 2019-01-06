module Ribosome.Msgpack.Util(
  string,
) where

import Data.ByteString.Internal (packChars)
import Data.MessagePack (Object(..))

string :: String -> Object
string = ObjectString . packChars
