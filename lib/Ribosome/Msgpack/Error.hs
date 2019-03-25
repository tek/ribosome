module Ribosome.Msgpack.Error where

import Ribosome.Msgpack.Util (Err)

newtype DecodeError =
  DecodeError Err
  deriving Show
