module Ribosome.Api.Register where

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Data.Register (Register)
import Ribosome.Data.RegisterType (RegisterType)
import qualified Ribosome.Data.RegisterType as RegisterType (RegisterType(..))
import Ribosome.Msgpack.Decode (MsgpackDecode(fromMsgpack))
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand)

setregAs ::
  NvimE e m =>
  MsgpackEncode a =>
  RegisterType ->
  Register ->
  a ->
  m ()
setregAs regType register text =
  vimCallFunction "setreg" [toMsgpack register, toMsgpack text, toMsgpack regType]

setreg ::
  NvimE e m =>
  Register ->
  Text ->
  m ()
setreg =
  setregAs RegisterType.Character

setregLine ::
  NvimE e m =>
  Register ->
  [Text] ->
  m ()
setregLine =
  setregAs RegisterType.Line

getregtype ::
  NvimE e m =>
  Register ->
  m RegisterType
getregtype register =
  vimCallFunction "getregtype" [toMsgpack register]

getregAs ::
  NvimE e m =>
  MsgpackDecode a =>
  Bool ->
  Register ->
  m a
getregAs list register =
  vimCallFunction "getreg" [toMsgpack register, toMsgpack (0 :: Int), toMsgpack list]

getreg ::
  NvimE e m =>
  Register ->
  m (Either Text [Text])
getreg register =
  withType =<< getregtype register
  where
    withType RegisterType.Line =
      Left <$> getregAs True register
    withType _ =
      Right <$> getregAs False register

getregList ::
  NvimE e m =>
  Register ->
  m [Text]
getregList =
  getregAs True
