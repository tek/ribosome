module Ribosome.Host.Test.ApiInfoTest where

import Polysemy.Test (Hedgehog, UnitTest, assertLeft, assertRight, evalEither, runTestAuto, (===))

import Ribosome.Host.Class.Msgpack.Error (renderError)
import Ribosome.Host.Data.ApiInfo (apiInfo, functions, types)
import Ribosome.Host.Data.ApiType (
  ApiPrim (Boolean, Dictionary, Float, Integer, LuaRef, Object, String, Void),
  ApiType (Array, Ext, Prim),
  parseApiType,
  )

checkType ::
  Member (Hedgehog IO) r =>
  ApiType ->
  ByteString ->
  Sem r ()
checkType target spec =
  assertRight target (parseApiType spec)

test_parseType :: UnitTest
test_parseType =
  runTestAuto do
    checkType (Array (Prim Integer) (Just 5)) "ArrayOf(Integer, 5)"
    checkType (Array (Array (Ext "Buffer") (Just 2)) (Just 3)) "ArrayOf(ArrayOf(Buffer, 2), 3)"
    checkType (Array (Prim Boolean) Nothing) "ArrayOf(Boolean)"
    checkType (Array (Prim Object) Nothing) "Array"
    checkType (Prim Float) "Float"
    checkType (Prim String) "String"
    checkType (Prim Dictionary) "Dictionary"
    checkType (Prim Object) "Object"
    checkType (Prim Void) "void"
    checkType (Prim LuaRef) "LuaRef"
    assertLeft err (first renderError (parseApiType "Booleans"))
    info <- evalEither =<< embed apiInfo
    255 === length info.functions
    3 === length info.types
  where
    err =
      "Decoding ApiType: Parsed (Prim Boolean) but got leftovers: s"
