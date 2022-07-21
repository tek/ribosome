-- |The effect 'Settings' abstracts Neovim variables
module Ribosome.Effect.Settings where

import Prelude hiding (get)

import Ribosome.Data.Setting (Setting)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)

-- |This effects abstracts Neovim variables with associated defaults.
data Settings :: Effect where
  -- |Get the value of the setting's Neovim variable or return the default if it is undefined.
  Get :: MsgpackDecode a => Setting a -> Settings m a
  -- |Set the value of the setting's Neovim variable.
  Update :: MsgpackEncode a => Setting a -> a -> Settings m ()

makeSem ''Settings

-- |Get the setting's value or return the supplied fallback value if the Neovim variable is undefined and the setting
-- has no default value.
or ::
  MsgpackDecode a =>
  Member (Settings !! SettingError) r =>
  a ->
  Setting a ->
  Sem r a
or a s =
  a <! get s

-- |Get 'Just' the setting's value or return 'Nothing' if the Neovim variable is undefined and the setting has no
-- default value.
maybe ::
  MsgpackDecode a =>
  Member (Settings !! SettingError) r =>
  Setting a ->
  Sem r (Maybe a)
maybe s =
  Nothing <! (Just <$> get s)
