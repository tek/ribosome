-- |Special command parameter that activates the bang modifier.
module Ribosome.Host.Data.Bang where

import Data.MessagePack (Object (ObjectBool))
import Exon (exon)

import Ribosome.Host.Class.Msgpack.Decode (pattern Msgpack, MsgpackDecode (fromMsgpack))

-- |When this type is used as a parameter of a command handler function, the command is declared with the @-bang@
-- option, and when invoked, the argument passed to the handler is v'Bang' if the user specified the @!@ and 'NoBang'
-- otherwise.
data Bang =
  -- |Bang was used.
  Bang
  |
  -- |Bang was not used.
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
