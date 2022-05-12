module Ribosome.Host.Data.Bang where

import Data.MessagePack (Object (ObjectBool))
import Exon (exon)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack), pattern Msgpack)

data Bang =
  Bang
  |
  NoBang
  deriving stock (Eq, Show)

instance MsgpackDecode Bang where
  fromMsgpack = \case
    ObjectBool True ->
      Right Bang
    ObjectBool False ->
      Right NoBang
    Msgpack (1 :: Int) ->
      Right Bang
    Msgpack (0 :: Int) ->
      Right NoBang
    o ->
      Left [exon|Bang arg must be Bool: #{show o}|]
