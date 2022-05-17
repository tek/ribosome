module Ribosome.Host.TH.Api.GenerateEffect where

import qualified Data.Kind as Kind
import Exon (exon)
import Language.Haskell.TH (
  Dec,
  DecQ,
  Name,
  Q,
  Quote (newName),
  Specificity (SpecifiedSpec),
  TyVarBndr (KindedTV),
  Type (AppT, ArrowT, ForallT, StarT, VarT),
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
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Host.Data.ApiType (ApiType, pattern PolyType)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.TH.Api.Generate (MethodSpec (MethodSpec), generateFromApi, reifyApiType)
import Ribosome.Host.TH.Api.Param (Param (Param, paramName))

msgpackDecodeConstraint :: ApiType -> Q (Maybe Type)
msgpackDecodeConstraint = \case
  PolyType ->
    Just <$> [t|MsgpackDecode $(varT (mkName "a"))|]
  _ ->
    pure Nothing

msgpackEncodeConstraint :: Param -> Q (Maybe Type)
msgpackEncodeConstraint = \case
  Param _ _ (Just p) ->
    Just <$> [t|MsgpackEncode $(varT p)|]
  Param _ _ Nothing ->
    pure Nothing

effReturnType :: ApiType -> Q (Maybe Name, Type)
effReturnType = \case
  PolyType -> do
    let n = mkName "a"
    pure (Just n, VarT (mkName "a"))
  a -> do
    t <- reifyApiType a
    pure (Nothing, t)

analyzeReturnType :: ApiType -> Q (Maybe Name, Type, Maybe Type)
analyzeReturnType tpe = do
  (n, rt) <- effReturnType tpe
  constraint <- msgpackDecodeConstraint tpe
  pure (n, rt, constraint)

effSig :: Name -> [Param] -> ApiType -> DecQ
effSig name params returnType = do
  stackName <- newName "r"
  stack <- varT stackName
  rpcConstraint <- [t|Member Rpc $(pure stack)|]
  (retTv, retType, decodeConstraint) <- analyzeReturnType returnType
  encodeConstraints <- traverse msgpackEncodeConstraint params
  semT <- [t|Sem|]
  stackKind <- [t|[(Kind.Type -> Kind.Type) -> Kind.Type -> Kind.Type]|]
  let
    paramType = \case
      Param _ _ (Just n) ->
        VarT n
      Param _ t Nothing ->
        t
    paramsType =
      foldr (AppT . AppT ArrowT . paramType) (AppT (AppT semT stack) retType) params
    constraints =
      rpcConstraint : maybeToList decodeConstraint <> catMaybes encodeConstraints
    paramTv = \case
      Param _ _ (Just n) ->
        Just n
      Param _ _ Nothing ->
        Nothing
    paramTvs =
      mapMaybe paramTv params
    tv n =
      KindedTV n SpecifiedSpec StarT
    stackTv =
      KindedTV stackName SpecifiedSpec stackKind
  sigD name (pure (ForallT ((tv <$> paramTvs) <> maybeToList (tv <$> retTv) <> [stackTv]) constraints paramsType))

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
