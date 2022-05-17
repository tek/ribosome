module Ribosome.Effect.Settings where

import Prelude hiding (get)

import Ribosome.Data.Setting (Setting)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)

data Settings :: Effect where
  Get :: MsgpackDecode a => Setting a -> Settings m a
  Update :: MsgpackEncode a => Setting a -> a -> Settings m ()

makeSem ''Settings

or ::
  MsgpackDecode a =>
  Member (Settings !! SettingError) r =>
  a ->
  Setting a ->
  Sem r a
or a s =
  a <! get s

maybe ::
  MsgpackDecode a =>
  Member (Settings !! SettingError) r =>
  Setting a ->
  Sem r (Maybe a)
maybe s =
  Nothing <! (Just <$> get s)
