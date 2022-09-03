{-# options_haddock prune #-}

-- |Errors for messagepack decoding.
module Ribosome.Host.Class.Msgpack.Error where

import Data.MessagePack (Object (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding.Error as UnicodeException
import Exon (exon)
import Log (Severity (Error))
import Numeric (showHex)
import Type.Reflection (typeRep)

import Ribosome.Host.Data.Report (Report (Report), Reportable (toReport))

-- |A decoding error in a field of a larger type.
--
-- May be nested arbitrarily deep.
data FieldError =
  FieldError Text
  |
  NestedFieldError DecodeError
  deriving stock (Eq, Show, Generic)

instance IsString FieldError where
  fromString =
    FieldError . fromString

-- |A messagepack decoding error.
data DecodeError =
  DecodeError {
    -- |The name of the type being decoded.
    mainType :: Text,
    -- |An error, potentially nested in other types.
    fieldError :: FieldError
  }
  deriving stock (Eq, Show, Generic)

compileError :: DecodeError -> (Text, Text)
compileError err =
  first (Text.intercalate " within " . reverse) (nest err)
  where
    nest DecodeError {..} =
      first (mainType :) (field fieldError)
    field = \case
      FieldError msg ->
        ([], msg)
      NestedFieldError nerr ->
        nest nerr

-- |Create a user-friendly message for a 'DecodeError'.
renderError :: DecodeError -> Text
renderError err =
  [exon|Decoding #{mainTypes}: #{fieldMsg}|]
  where
    (mainTypes, fieldMsg) =
      compileError err

instance Reportable DecodeError where
  toReport err =
    Report msg [msg] Error
    where
      msg =
        renderError err

-- |Convert a 'FieldError' in a 'Left' to a 'DecodeError' by adding the type name via 'Typeable'.
toDecodeError ::
  ∀ a .
  Typeable a =>
  Either FieldError a ->
  Either DecodeError a
toDecodeError =
  first (DecodeError (show (typeRep @a)))

-- |Create a @'Left' 'DecodeError'@ from a 'Text' by adding the type name via 'Typeable'.
decodeError ::
  ∀ a .
  Typeable a =>
  Text ->
  Either DecodeError a
decodeError msg =
  toDecodeError (Left (FieldError msg))

symbolText ::
  ∀ a .
  KnownSymbol a =>
  Text
symbolText =
  toText (symbolVal (Proxy @a))

describe :: Object -> Text
describe = \case
  ObjectNil -> "Nil"
  ObjectUInt _ -> "UInt"
  ObjectInt _ -> "Int"
  ObjectBool _ -> "Bool"
  ObjectFloat _ -> "Float"
  ObjectDouble _ -> "Double"
  ObjectString _ -> "String"
  ObjectBinary _ -> "Binary"
  ObjectArray _ -> "Array"
  ObjectMap _ -> "Map"
  ObjectExt _ _ -> "Ext"

incompatibleShapeError ::
  Text ->
  Text ->
  FieldError
incompatibleShapeError target got =
  FieldError [exon|Got #{got} for #{target}|]

incompatibleShape ::
  Text ->
  Text ->
  Either FieldError a
incompatibleShape target got =
  Left (incompatibleShapeError target got)

incompatibleCon ::
  Text ->
  Object ->
  Either FieldError a
incompatibleCon target o =
  incompatibleShape target (describe o)

-- |Create a 'FieldError' for a field when the 'Object' constructor is wrong, using 'Typeable' to obtain the type name.
incompatible ::
  ∀ a .
  Typeable a =>
  Object ->
  Either FieldError a
incompatible =
  incompatibleCon (show (typeRep @a))

-- |Create a 'DecodeError' for a type when the 'Object' constructor is wrong, using 'Typeable' to obtain the type name.
decodeIncompatible ::
  ∀ a .
  Typeable a =>
  Object ->
  Either DecodeError a
decodeIncompatible o =
  decodeError [exon|Got #{describe o}|]

utf8Error :: UnicodeException -> FieldError
utf8Error = \case
  UnicodeException.DecodeError _ (Just w) ->
    FieldError [exon|Invalid byte \x#{toText (showHex w "")}|]
  UnicodeException.DecodeError _ Nothing ->
    FieldError "Incomplete input"
  _ ->
    FieldError "Impossible encode error"
