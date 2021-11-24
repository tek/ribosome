{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ribosome.Plugin.TH.Command where

import Control.Exception (throw)
import Data.Aeson (FromJSON, eitherDecodeStrict)
import qualified Data.ByteString as ByteString (intercalate)
import Data.MessagePack (Object(ObjectArray))
import Prettyprinter (Pretty(..))
import Language.Haskell.TH
import Neovim.Exceptions (NeovimException(ErrorMessage))
import Neovim.Plugin.Classes (CommandArguments, CommandOption(..), Synchronous, mkCommandOptions)

import Ribosome.Msgpack.Decode (fromMsgpack)
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Msgpack.Util (Err)
import Ribosome.Plugin.TH.Handler (
  RpcDef(RpcDef),
  RpcDefDetail(RpcCommand),
  argsCase,
  decodedCallSequence,
  functionParamTypes,
  handlerCall,
  lambdaNames,
  listParamsPattern,
  )

data CmdParamType =
  PrimParam
  |
  DataParam
  |
  ListParam
  deriving stock (Eq, Show)

data CmdParams =
  ZeroParams
  |
  OneParam Bool CmdParamType
  |
  OnlyPrims Int
  |
  DataPlus Int
  deriving stock (Eq, Show)

data HandlerParams =
  HandlerParams {
    handlerHasArgsParam :: Bool,
    handlerCmdParams :: CmdParams
    }
  deriving stock (Eq, Show)

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
  [MatchQ] ->
  String ->
  Name ->
  Name ->
  Name ->
  [Name] ->
  Bool ->
  ExpQ
primDispatch extraCases rpcName argsName handlerName cmdArgsName paramNames hasArgsParam =
  caseE (varE argsName) (extraCases ++ [matching, invalidArgs])
  where
    matching =
      match (primArgPattern paramNames) (normalB $ decodedCallSequence handlerName vars) []
    invalidArgs =
      match wildP (normalB [|invalidArgCount $ $(nameLit) <> "(" <> show $(varE argsName) <> ")"|]) []
    vars =
      varE <$> params
    params =
      if hasArgsParam then cmdArgsName : paramNames else paramNames
    nameLit =
      litE (StringL rpcName)

primDispatchStrict ::
  String ->
  Name ->
  Name ->
  Name ->
  [Name] ->
  Bool ->
  ExpQ
primDispatchStrict =
  primDispatch []

primDispatchMaybe ::
  String ->
  Name ->
  Name ->
  Name ->
  [Name] ->
  Bool ->
  ExpQ
primDispatchMaybe rpcName argsName handlerName  =
  primDispatch [nothingCase] rpcName argsName handlerName
  where
    nothingCase =
      match (listP []) (normalB (appE [|pure|] (appE (varE handlerName) [|Nothing|]))) []

primDispatchList ::
  String ->
  Name ->
  Name ->
  Name ->
  [Name] ->
  Bool ->
  ExpQ
primDispatchList _ argsName handlerName cmdArgsName _ hasArgsParam = do
  listArgName <- newName "as"
  caseE (varE argsName) [listCase listArgName]
  where
    listCase listArgName =
      match (varP listArgName) (normalB $ handlerCall handlerName (params listArgName)) []
    params listArgName =
      if hasArgsParam then [[|fromMsgpack $(varE cmdArgsName)|], decodedList listArgName] else [decodedList listArgName]
    decodedList listArgName =
      [|traverse fromMsgpack $(varE listArgName)|]

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
    normalize rpcName args =
      shapeError $ rpcName <> " (" <> show args <> ")"

normalizeArgsPlus ::
  Monad m =>
  ArgNormalizer m
normalizeArgsPlus =
  ArgNormalizer normalize
  where
    normalize _ [cmdArgs, first', ObjectArray rest] =
      return (cmdArgs, first' : rest)
    normalize rpcName args =
      shapeError $ rpcName <> " (" <> show args <> ")"

normalizeArgs ::
  CmdParams ->
  ExpQ
normalizeArgs ZeroParams =
  [|normalizeArgsFlat|]
normalizeArgs (OnlyPrims 1) =
  [|normalizeArgsFlat|]
normalizeArgs (OneParam _ PrimParam) =
  [|normalizeArgsFlat|]
normalizeArgs (OneParam _ ListParam) =
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
  (Name -> Name -> [Name] -> Bool -> ExpQ) ->
  ExpQ
command rpcName handlerName paramNames (HandlerParams hasCmdArgs cmdParams) argsPattern dispatch = do
  cmdArgsName <- newName "cmdArgs"
  [|rpc $(nameLit) $(normalizeArgs cmdParams) $(handler cmdArgsName)|]
  where
    handler cmdArgsName =
      lamE [firstParam cmdArgsName, argsPattern] (dispatch handlerName cmdArgsName paramNames hasCmdArgs)
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
  Bool ->
  Bool ->
  ExpQ
primCommand rpcName handlerName paramNames handlerPar isMaybe isL = do
  argsName <- newName "args"
  command rpcName handlerName paramNames handlerPar (varP argsName) (dispatch rpcName argsName)
  where
    dispatch | isMaybe = primDispatchMaybe
      | isL = primDispatchList
      | otherwise = primDispatchStrict

jsonCommand ::
  String ->
  Name ->
  [Name] ->
  HandlerParams ->
  Bool ->
  ExpQ
jsonCommand rpcName handlerName paramNames handlerPar _ = do
  restName <- newName "rest"
  command rpcName handlerName paramNames handlerPar (jsonArgPattern paramNames restName) (jsonDispatch restName)

commandImplementation :: String -> Name -> HandlerParams -> ExpQ
commandImplementation rpcName handlerName hps@(HandlerParams _ params) =
  forParams params
  where
    forParams ZeroParams =
      primCommand rpcName handlerName [] hps False False
    forParams (OnlyPrims paramCount) = do
      paramNames <- lambdaNames paramCount
      primCommand rpcName handlerName paramNames hps False False
    forParams (DataPlus paramCount) = do
      paramNames <- lambdaNames paramCount
      jsonCommand rpcName handlerName paramNames hps False
    forParams (OneParam isMaybe DataParam) =
      jsonCommand rpcName handlerName [] hps isMaybe
    forParams (OneParam isMaybe PrimParam) = do
      paramNames <- lambdaNames 1
      primCommand rpcName handlerName paramNames hps isMaybe False
    forParams (OneParam isMaybe ListParam) = do
      paramNames <- lambdaNames 1
      primCommand rpcName handlerName paramNames hps isMaybe True

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

isList :: Type -> Bool
isList (AppT ListT _) =
  True
isList _ =
  False

isMaybeType :: Name -> Q Bool
isMaybeType tpe =
  (ConT tpe ==) <$> [t|Maybe|]

analyzeMaybeCmdParam :: Type -> Q (Bool, Type)
analyzeMaybeCmdParam (AppT (ConT tcon) tpe) =
  (, tpe) <$> isMaybeType tcon
analyzeMaybeCmdParam a =
  return (False, a)

analyzeCmdParams :: [Type] -> Q CmdParams
analyzeCmdParams =
  check . reverse
  where
    check [a] = do
      (isMaybe, tpe) <- analyzeMaybeCmdParam a
      isD <- isJsonDecodable tpe
      return $ OneParam isMaybe (singleParam isD (isList tpe))
    check (a : rest) = do
      isD <- isJsonDecodable a
      return $ if isD then DataPlus (length rest) else OnlyPrims (length rest + 1)
    check [] =
      return ZeroParams
    singleParam _ True =
      ListParam
    singleParam True _ =
      DataParam
    singleParam _ _ =
      PrimParam

cmdNargs :: CmdParams -> CommandOption
cmdNargs ZeroParams =
  CmdNargs "0"
cmdNargs (OnlyPrims 1) =
  CmdNargs "1"
cmdNargs (OneParam False PrimParam) =
  CmdNargs "1"
cmdNargs (OneParam True PrimParam) =
  CmdNargs "?"
cmdNargs (OneParam _ ListParam) =
  CmdNargs "*"
cmdNargs _ =
  CmdNargs "+"

amendSync :: Synchronous -> [CommandOption] -> [CommandOption]
amendSync _ options | any isSync options =
  options
  where
    isSync (CmdSync _) =
      True
    isSync _ =
      False
amendSync sync options =
  CmdSync sync : options

rpcCommand :: String -> Name -> HandlerParams -> Synchronous -> [CommandOption] -> ExpQ
rpcCommand rpcName funcName hps@(HandlerParams _ params) sync opts = do
  fun <- commandImplementation rpcName funcName hps
  [|RpcDef (RpcCommand $ mkCommandOptions (nargs : amendSync sync opts)) $((litE (StringL rpcName))) $(return fun)|]
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
