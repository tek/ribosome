{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Plugin.TH.Command where

import Control.Exception (throw)
import Control.Monad ((<=<))
import Data.Aeson (FromJSON, eitherDecodeStrict)
import qualified Data.ByteString as ByteString (intercalate)
import Data.Either.Combinators (mapLeft)
import Data.MessagePack (Object(ObjectArray))
import Data.Text.Prettyprint.Doc (Pretty(..))
import Language.Haskell.TH
import Neovim.Exceptions (NeovimException(ErrorMessage))
import Neovim.Plugin.Classes (
  CommandArguments,
  CommandOption(..),
  mkCommandOptions,
  )

import Ribosome.Msgpack.Decode (fromMsgpack)
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Msgpack.Util (Err)
import Ribosome.Plugin.TH.Handler (
  RpcDef(RpcDef),
  RpcDefDetail(RpcCommand),
  argsCase,
  decodedCallSequence,
  functionParamTypes,
  lambdaNames,
  listParamsPattern,
  )

data CmdParams =
  ZeroParams
  |
  OnlyPrims Int
  |
  OnlyData
  |
  DataPlus Int
  deriving (Eq, Show)

data HandlerParams =
  HandlerParams {
    handlerHasArgsParam :: Bool,
    handlerCmdParams :: CmdParams
    }
  deriving (Eq, Show)

colon :: Name
colon =
  mkName ":"

colonE :: ExpQ
colonE =
  varE colon

colonP :: PatQ
colonP =
  varP colon

cmdArgsCase :: Name -> [Name] -> Q Match
cmdArgsCase handlerName paramNames =
  argsCase handlerName (listParamsPattern (mkName "_" : paramNames)) paramNames

decodeJson :: FromJSON a => [Object] -> Either Err a
decodeJson =
  mapLeft pretty . eitherDecodeStrict . ByteString.intercalate " " <=< traverse fromMsgpack

primDispatch ::
  String ->
  Name ->
  Name ->
  Name ->
  [Name] ->
  Bool ->
  ExpQ
primDispatch rpcName argsName handlerName cmdArgsName paramNames hasArgsParam =
  caseE (varE argsName) [matching, invalidArgs]
  where
    matching =
      match (primArgPattern paramNames) (normalB $ decodedCallSequence handlerName vars) []
    invalidArgs =
      match wildP (normalB [|invalidArgCount $(nameLit)|]) []
    vars = varE <$> params
    params =
      if hasArgsParam then cmdArgsName : paramNames else paramNames
    nameLit =
      litE (StringL rpcName)

jsonDispatch ::
  Name ->
  Name ->
  Name ->
  [Name] ->
  Bool ->
  ExpQ
jsonDispatch restName handlerName cmdArgsName paramNames hasArgsParam =
  infixApp prims [|(<*>)|] decodedRest
  where
    prims = decodedCallSequence handlerName vars
    vars = varE <$> params
    params = if hasArgsParam then cmdArgsName : paramNames else paramNames
    decodedRest = [|decodeJson $(varE restName)|]

primArgPattern :: [Name] -> PatQ
primArgPattern paramNames =
  foldr f (listP []) (varP <$> paramNames)
  where
    f a = infixP a (mkName ":")

jsonArgPattern :: [Name] -> Name -> PatQ
jsonArgPattern paramNames restName =
  foldr f (varP restName) (varP <$> paramNames)
  where
    f a = infixP a (mkName ":")

newtype ArgNormalizer m =
  ArgNormalizer (Text -> [Object] -> m (Object, [Object]))

shapeError :: Text -> m a
shapeError =
  throw . ErrorMessage . pretty . errorMessage
  where
    errorMessage =
      ("Bad argument shape for rpc command: " <>)

normalizeArgsFlat ::
  Monad m =>
  ArgNormalizer m
normalizeArgsFlat =
  ArgNormalizer normalize
  where
    normalize _ (cmdArgs : rest) =
      return (cmdArgs, rest)
    normalize rpcName _ =
      shapeError rpcName

normalizeArgsPlus ::
  Monad m =>
  ArgNormalizer m
normalizeArgsPlus =
  ArgNormalizer normalize
  where
    normalize _ [cmdArgs, first', ObjectArray rest] =
      return (cmdArgs, first' : rest)
    normalize rpcName _ =
      shapeError rpcName

normalizeArgs ::
  CmdParams ->
  ExpQ
normalizeArgs ZeroParams =
  [|normalizeArgsFlat|]
normalizeArgs (OnlyPrims 1) =
  [|normalizeArgsFlat|]
normalizeArgs _ =
  [|normalizeArgsPlus|]

rpc ::
  Monad m =>
  MsgpackEncode a =>
  Text ->
  ArgNormalizer m ->
  (Object -> [Object] -> Either Err (m a)) ->
  [Object] ->
  m Object
rpc rpcName (ArgNormalizer normalize) dispatch =
  toMsgpack <$$> decodeResult . uncurry dispatch <=< normalize rpcName
  where
    decodeResult =
      either (throw . ErrorMessage) id

invalidArgCount :: String -> m a
invalidArgCount =
  throw . ErrorMessage . pretty . (msg <>)
  where
    msg =
      "Wrong number of arguments for rpc handler: "

command ::
  String ->
  Name ->
  [Name] ->
  HandlerParams ->
  PatQ ->
  (Name -> Name -> [Name] -> Bool ->ExpQ) ->
  ExpQ
command rpcName handlerName paramNames (HandlerParams hasCmdArgs cmdParams) argsPattern dispatch = do
  cmdArgsName <- newName "cmdArgs"
  let
    handler =
      lamE [firstParam cmdArgsName, argsPattern] (dispatch handlerName cmdArgsName paramNames hasCmdArgs)
  [|rpc $(nameLit) $(normalizeArgs cmdParams) $(handler)|]
  where
    firstParam cmdArgsName =
      if hasCmdArgs then varP cmdArgsName
      else wildP
    nameLit =
      litE (StringL rpcName)

primCommand ::
  String ->
  Name ->
  [Name] ->
  HandlerParams ->
  ExpQ
primCommand rpcName handlerName paramNames handlerPar = do
  argsName <- newName "args"
  command rpcName handlerName paramNames handlerPar (varP argsName) (primDispatch rpcName argsName)

jsonCommand :: String -> Name -> [Name] -> HandlerParams -> ExpQ
jsonCommand rpcName handlerName paramNames handlerPar = do
  restName <- newName "rest"
  command rpcName handlerName paramNames handlerPar (jsonArgPattern paramNames restName) (jsonDispatch restName)

commandImplementation :: String -> Name -> HandlerParams -> ExpQ
commandImplementation rpcName handlerName hps@(HandlerParams _ params) =
  forParams params
  where
    forParams ZeroParams =
      primCommand rpcName handlerName [] hps
    forParams (OnlyPrims paramCount) = do
      paramNames <- lambdaNames paramCount
      primCommand rpcName handlerName paramNames hps
    forParams (DataPlus paramCount) = do
      paramNames <- lambdaNames paramCount
      jsonCommand rpcName handlerName paramNames hps
    forParams OnlyData =
      jsonCommand rpcName handlerName [] hps

isRecord :: Info -> Bool
isRecord (TyConI (DataD _ _ _ _ [RecC _ _] _)) =
  True
isRecord _ =
  False

isJsonDecodable :: Type -> Q Bool
isJsonDecodable (ConT name) =
  isRecord <$> reify name
isJsonDecodable _ =
  return False

analyzeCmdParams :: [Type] -> Q CmdParams
analyzeCmdParams =
  check . reverse
  where
    check [a] = do
      isD <- isJsonDecodable a
      return $ if isD then OnlyData else OnlyPrims 1
    check (a : rest) = do
      isD <- isJsonDecodable a
      return $ if isD then DataPlus (length rest) else OnlyPrims (length rest + 1)
    check [] =
      return ZeroParams

cmdNargs :: CmdParams -> CommandOption
cmdNargs ZeroParams =
  CmdNargs "0"
cmdNargs (OnlyPrims 1) =
  CmdNargs "1"
cmdNargs _ =
  CmdNargs "+"

rpcCommand :: String -> Name -> HandlerParams -> [CommandOption] -> ExpQ
rpcCommand rpcName funcName hps@(HandlerParams _ params) opts = do
  fun <- commandImplementation rpcName funcName hps
  [|RpcDef (RpcCommand $ mkCommandOptions (nargs : opts)) $((litE (StringL rpcName))) $(return fun)|]
  where
    nargs = cmdNargs params

removeArgsParam :: [Type] -> Q (Bool, [Type])
removeArgsParam [] =
  return (False, [])
removeArgsParam (p1 : rest) = do
  argsType <- [t|CommandArguments|]
  return $ if p1 == argsType then (True, rest) else (False, p1 : rest)

handlerParams :: Name -> Q HandlerParams
handlerParams name = do
  types <- functionParamTypes name
  (hasArgsParam, userTypes) <- removeArgsParam types
  cp <- analyzeCmdParams userTypes
  return $ HandlerParams hasArgsParam cp
