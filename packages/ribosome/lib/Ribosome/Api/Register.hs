-- |API functions for registers.
module Ribosome.Api.Register where

import Ribosome.Data.Register (Register)
import qualified Ribosome.Data.Register as Register (Register (..))
import Ribosome.Data.RegisterType (RegisterType)
import qualified Ribosome.Data.RegisterType as RegisterType (RegisterType (..))
import Ribosome.Host.Api.Effect (vimCallFunction)
import Ribosome.Host.Class.Msgpack.Array (msgpackArray)
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Host.Effect.Rpc (Rpc)

-- |The special register referring to the primary X11 selection.
starRegister :: Register
starRegister =
  Register.Special "*"

-- |The special register referring to the clipboard X11 selection.
plusRegister :: Register
plusRegister =
  Register.Special "+"

-- |The special register referring to the unnamed register.
unnamedRegister :: Register
unnamedRegister =
  Register.Special "\""

-- |Set a Neovim register's contents, using the specified type.
--
-- A register's type determines whether the contents are supposed to be pasted as lines or characters, for example.
setregAs ::
  Member Rpc r =>
  MsgpackEncode a =>
  RegisterType ->
  Register ->
  a ->
  Sem r ()
setregAs regType register text =
  vimCallFunction "setreg" (msgpackArray register text regType)

-- |Set a Neovim register's contents as inline characters.
setreg ::
  Member Rpc r =>
  Register ->
  Text ->
  Sem r ()
setreg =
  setregAs RegisterType.Character

-- |Set a Neovim register's contents as whole lines.
setregLine ::
  Member Rpc r =>
  Register ->
  [Text] ->
  Sem r ()
setregLine =
  setregAs RegisterType.Line

-- |Get the type of a register's contents.
getregtype ::
  Member Rpc r =>
  Register ->
  Sem r RegisterType
getregtype register =
  vimCallFunction "getregtype" [toMsgpack register]

-- |Get a Neovim register's contents, using the specified type.
--
-- A register's type determines whether the contents are supposed to be pasted as lines or characters, for example.
getregAs ::
  Member Rpc r =>
  MsgpackDecode a =>
  Bool ->
  Register ->
  Sem r a
getregAs list register =
  vimCallFunction "getreg" (msgpackArray register (0 :: Int) list)

-- |Get a Neovim register's contents as inline characters.
getreg ::
  Member Rpc r =>
  Register ->
  Sem r (Either [Text] Text)
getreg register =
  withType =<< getregtype register
  where
    withType RegisterType.Line =
      Left <$> getregAs True register
    withType _ =
      Right <$> getregAs False register

-- |Get a Neovim register's contents as whole lines.
getregLines ::
  Member Rpc r =>
  Register ->
  Sem r [Text]
getregLines =
  getregAs True
