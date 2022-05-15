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

starRegister :: Register
starRegister =
  Register.Special "*"

unnamedRegister :: Register
unnamedRegister =
  Register.Special "\""

setregAs ::
  Member Rpc r =>
  MsgpackEncode a =>
  RegisterType ->
  Register ->
  a ->
  Sem r ()
setregAs regType register text =
  vimCallFunction "setreg" (msgpackArray register text regType)

setreg ::
  Member Rpc r =>
  Register ->
  Text ->
  Sem r ()
setreg =
  setregAs RegisterType.Character

setregLine ::
  Member Rpc r =>
  Register ->
  [Text] ->
  Sem r ()
setregLine =
  setregAs RegisterType.Line

getregtype ::
  Member Rpc r =>
  Register ->
  Sem r RegisterType
getregtype register =
  vimCallFunction "getregtype" [toMsgpack register]

getregAs ::
  Member Rpc r =>
  MsgpackDecode a =>
  Bool ->
  Register ->
  Sem r a
getregAs list register =
  vimCallFunction "getreg" (msgpackArray register (0 :: Int) list)

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

getregList ::
  Member Rpc r =>
  Register ->
  Sem r [Text]
getregList =
  getregAs True
