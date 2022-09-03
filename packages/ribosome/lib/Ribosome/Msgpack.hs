-- |Tools for writing instances of 'MsgpackDecode'.
module Ribosome.Msgpack (
  module Ribosome.Host.Class.Msgpack.Decode,
  module Ribosome.Host.Class.Msgpack.Encode,
  module Ribosome.Host.Class.Msgpack.Error,
  module Ribosome.Host.Class.Msgpack.Util,
  module Ribosome.Host.Class.Msgpack.Map,
  module Ribosome.Host.Class.Msgpack.Array,
) where

import Ribosome.Host.Class.Msgpack.Array (msgpackArray)
import Ribosome.Host.Class.Msgpack.Decode (MissingKey (..), pattern Msgpack, MsgpackDecode (..))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (..))
import Ribosome.Host.Class.Msgpack.Error (
  DecodeError (..),
  FieldError (..),
  decodeError,
  decodeIncompatible,
  incompatible,
  renderError,
  toDecodeError,
  )
import Ribosome.Host.Class.Msgpack.Map (msgpackMap)
import Ribosome.Host.Class.Msgpack.Util (
  pattern MsgpackString,
  byteStringField,
  decodeByteString,
  decodeFractional,
  decodeIntegral,
  decodeString,
  decodeUtf8Lenient,
  fractionalField,
  integralField,
  readField,
  stringField,
  )
