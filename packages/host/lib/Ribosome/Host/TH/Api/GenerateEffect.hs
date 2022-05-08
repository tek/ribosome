module Ribosome.Host.TH.Api.GenerateEffect where

import Exon (exon)
import Language.Haskell.TH (
  Dec,
  DecQ,
  Name,
  Q,
  Quote (newName),
  Type (AppT, ArrowT, ForallT, VarT),
  TypeQ,
  appE,
  clause,
  funD,
  mkName,
  nameBase,
  normalB,
  sigD,
  varE,
  varP,
  varT,
  )
import Prelude hiding (Type)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Data.ApiType (ApiPrim (Object), ApiType (Prim))
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.TH.Api.Generate (MethodSpec (MethodSpec), generateFromApi, reifyApiType)
import Ribosome.Host.TH.Api.Param (Param (paramName, Param))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode(toMsgpack))

msgpackDecodeConstraint :: ApiType -> Q (Maybe Type)
msgpackDecodeConstraint = \case
  Prim Object ->
    Just <$> [t|MsgpackDecode $(varT (mkName "a"))|]
  _ ->
    pure Nothing

msgpackEncodeConstraint :: Param -> Q (Maybe Type)
msgpackEncodeConstraint = \case
  Param _ _ (Just p) ->
    Just <$> [t|MsgpackEncode $(varT p)|]
  Param _ _ Nothing ->
    pure Nothing

newT :: String -> TypeQ
newT =
  varT <=< newName

effReturnType :: ApiType -> Q Type
effReturnType = \case
  Prim Object ->
    pure (VarT (mkName "a"))
  a ->
    reifyApiType a

analyzeReturnType :: ApiType -> Q (Type, Maybe Type)
analyzeReturnType tpe = do
  rt <- effReturnType tpe
  constraint <- msgpackDecodeConstraint tpe
  pure (rt, constraint)

effSig :: Name -> [Param] -> ApiType -> DecQ
effSig name params returnType = do
  stack <- newT "r"
  rpcConstraint <- [t|Member Rpc $(pure stack)|]
  (retType, decodeConstraint) <- analyzeReturnType returnType
  encodeConstraints <- traverse msgpackEncodeConstraint params
  semT <- [t|Sem|]
  let
    paramsType =
      foldr (AppT . AppT ArrowT . paramType) (AppT (AppT semT stack) retType) params
    paramType = \case
      Param _ _ (Just n) ->
        VarT n
      Param _ t Nothing ->
        t
    constraints =
      rpcConstraint : maybeToList decodeConstraint <> catMaybes encodeConstraints
  sigD name (pure (ForallT [] constraints paramsType))

effBody :: Name -> [Param] -> DecQ
effBody name params =
  funD name [clause (varP <$> names) (normalB effectCons) []]
  where
    effectCons =
      appE [|Rpc.sync|] args
    args =
      foldl appE (varE (mkName [exon|RpcData.#{nameBase name}|])) (paramE <$> params)
    names =
      paramName <$> params
    paramE = \case
      Param n _ p ->
        fromMaybe id (appE [e|toMsgpack|] <$ p) (varE n)

genMethod :: MethodSpec -> Q [Dec]
genMethod (MethodSpec _ name params returnType) = do
  sig <- effSig name params returnType
  body <- effBody name params
  pure [sig, body]

generateEffect :: Q [Dec]
generateEffect =
  generateFromApi genMethod Nothing
