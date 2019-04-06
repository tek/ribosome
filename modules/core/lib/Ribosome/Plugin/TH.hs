{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Plugin.TH where

import Control.Exception (throw)
import Control.Monad (replicateM, (<=<))
import Data.Aeson (FromJSON, eitherDecodeStrict)
import qualified Data.ByteString as ByteString (intercalate)
import Data.Either.Combinators (mapLeft)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, maybeToList)
import Data.MessagePack (Object)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..))
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import Neovim.Exceptions (NeovimException(ErrorMessage))
import Neovim.Plugin.Classes (
  CommandOption(..),
  CommandOptions,
  RangeSpecification(..),
  Synchronous(..),
  mkCommandOptions,
  )

import Ribosome.Data.String (capitalize)
import Ribosome.Msgpack.Decode (fromMsgpack)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Msgpack.Util (Err)

data RpcHandlerConfig =
  RpcHandlerConfig {
    rhcSync :: Synchronous,
    rhcName :: Maybe String,
    rhcCmd :: Maybe [CommandOption]
  }
  deriving (Eq, Show)

defaultRpcHandlerConfig :: RpcHandlerConfig
defaultRpcHandlerConfig =
  RpcHandlerConfig Async Nothing Nothing

data RpcDefDetail =
  RpcFunction { rfSync :: Synchronous }
  |
  RpcCommand { rcOptions :: CommandOptions }

data RpcDef m =
  RpcDef {
    rdDetail :: RpcDefDetail,
    rdName :: String,
    rdHandler :: [Object] -> m Object
  }

instance Lift Synchronous where
  lift Sync = [|Sync|]
  lift Async = [|Async|]

instance Lift RangeSpecification where
  lift CurrentLine =
    [|CurrentLine|]
  lift WholeFile =
    [|WholeFile|]
  lift (RangeCount a) =
    [|RangeCount a|]

instance Lift CommandOption where
  lift (CmdSync a) =
    [|CmdSync a|]
  lift CmdRegister =
    [|CmdRegister|]
  lift (CmdNargs a) =
    [|CmdNargs a|]
  lift (CmdRange a) =
    [|CmdRange a|]
  lift (CmdCount a) =
    [|CmdCount a|]
  lift CmdBang =
    [|CmdBang|]

unfoldFunctionParams :: Type -> [Type]
unfoldFunctionParams (ForallT _ _ t) =
  unfoldFunctionParams t
unfoldFunctionParams (AppT (AppT ArrowT t) r) =
  t : unfoldFunctionParams r
unfoldFunctionParams _ = []

functionParamTypes :: Name -> Q [Type]
functionParamTypes name =
  reify name <&> \case
    (VarI _ functionType _) -> unfoldFunctionParams functionType
    _ -> error $ "rpc handler `" ++ show name ++ "` is not a function"

data CmdParams =
  ZeroParams
  |
  OnlyPrims Int
  |
  OnlyData
  |
  DataPlus Int
  deriving (Eq, Show)

errorCase :: Name -> Q Match
errorCase name =
  match wildP (normalB [|throw . ErrorMessage . pretty $ errMsg ++ $(litE (StringL (nameBase name))) |]) []
  where
    errMsg = "Wrong number of arguments for rpc handler: " :: String

failedEvaluation :: Q Match
failedEvaluation = do
  e <- newName "e"
  match (conP (mkName "Left") [varP e]) (normalB [|throw . ErrorMessage $ ($(varE e) :: Doc AnsiStyle)|]) []

successfulEvaluation :: Q Match
successfulEvaluation = do
  action <- newName "action"
  match (conP (mkName "Right") [varP action]) (normalB [|toMsgpack <$> $(varE action)|]) []

dispatchCase :: PatQ -> ExpQ -> Q Match
dispatchCase params dispatch =
  match params (normalB (caseE dispatch resultCases)) []
  where
    resultCases = [successfulEvaluation, failedEvaluation]

decodedCallSequence :: Name -> [ExpQ] -> ExpQ
decodedCallSequence handlerName =
  foldl decodeSeq [|pure $(varE handlerName)|]
  where
    decodeSeq z a = infixApp z [|(<*>)|] [|fromMsgpack $(a)|]

argsCase :: Name -> PatQ -> [Name] -> Q Match
argsCase handlerName params paramNames =
  dispatchCase params dispatch
  where
    dispatch = decodedCallSequence handlerName vars
    vars = varE <$> paramNames

cmdArgsCase :: Name -> [Name] -> Q Match
cmdArgsCase handlerName paramNames =
  argsCase handlerName (listParamsPattern (mkName "_" : paramNames)) paramNames

rpcLambda :: Q Match -> Maybe (Q Match) -> ExpQ
rpcLambda matchingArgsCase errorCase' = do
  args <- newName "args"
  lamE [varP args] (caseE (varE args) (matchingArgsCase : maybeToList errorCase'))

rpcLambdaWithErrorCase :: Name -> Q Match -> ExpQ
rpcLambdaWithErrorCase funcName matchingArgsCase =
  rpcLambda matchingArgsCase $ Just (errorCase funcName)

rpcLambdaWithoutErrorCase :: Q Match -> ExpQ
rpcLambdaWithoutErrorCase matchingArgsCase =
  rpcLambda matchingArgsCase Nothing

listParamsPattern :: [Name] -> PatQ
listParamsPattern =
  listP . (varP <$>)

unconsParamsPattern :: [Name] -> Name -> PatQ
unconsParamsPattern paramNames restName =
  foldr f (varP restName) (varP <$> paramNames)
  where
    f a = infixP a (mkName ":")

lambdaNames :: Int -> Q [Name]
lambdaNames count =
  replicateM count (newName "a")

functionImplementation :: Name -> ExpQ
functionImplementation name = do
  paramTypes <- functionParamTypes name
  paramNames <- lambdaNames (length paramTypes)
  rpcLambdaWithErrorCase name (argsCase name (listParamsPattern paramNames) paramNames)

decodeJson :: FromJSON a => [Object] -> Either Err a
decodeJson =
  mapLeft pretty . eitherDecodeStrict . ByteString.intercalate " " <=< traverse fromMsgpack

dataDispatch :: Name -> [Name] -> Name -> ExpQ
dataDispatch handlerName paramNames restName =
  infixApp prims [|(<*>)|] decodedRest
  where
    prims = decodedCallSequence handlerName vars
    vars = varE <$> paramNames
    decodedRest = [|decodeJson $(varE restName)|]

dataArgCommand :: Name -> [Name] -> ExpQ
dataArgCommand name paramNames = do
  restName <- newName "rest"
  mkLambda (dispatchCase (unconsParamsPattern paramNames restName) (dataDispatch name paramNames restName))
  where
    mkLambda = if null paramNames then rpcLambdaWithoutErrorCase else rpcLambdaWithErrorCase name

commandImplementation :: CmdParams -> Name -> ExpQ
commandImplementation ZeroParams name =
  rpcLambdaWithErrorCase name (cmdArgsCase name [])
commandImplementation (OnlyPrims paramCount) name = do
  paramNames <- lambdaNames paramCount
  rpcLambdaWithErrorCase name (cmdArgsCase name paramNames)
commandImplementation (DataPlus paramCount) name = do
  paramNames <- lambdaNames paramCount
  dataArgCommand name paramNames
commandImplementation OnlyData name =
  dataArgCommand name []

isJsonDecodable :: Type -> Q Bool
isJsonDecodable (ConT name) = do
  (ConT jsonClass) <- [t|FromJSON|]
  isInstance jsonClass [ConT name]
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

cmdParams :: Name -> Q CmdParams
cmdParams =
  analyzeCmdParams <=< functionParamTypes

cmdNargs :: CmdParams -> CommandOption
cmdNargs ZeroParams =
  CmdNargs "0"
cmdNargs (OnlyPrims 1) =
  CmdNargs "1"
cmdNargs _ =
  CmdNargs "+"

rpcFunction :: String -> Synchronous -> Name -> ExpQ
rpcFunction name sync funcName = do
  fun <- functionImplementation funcName
  [|RpcDef (RpcFunction sync) $((litE (StringL name))) $(return fun)|]

rpcCommand :: String -> Name -> [CommandOption] -> ExpQ
rpcCommand name funcName opts = do
  params <- cmdParams funcName
  fun <- commandImplementation params funcName
  let nargs = cmdNargs params
  [|RpcDef (RpcCommand $ mkCommandOptions (nargs : opts)) $((litE (StringL name))) $(return fun)|]

rpcHandler :: (RpcHandlerConfig -> RpcHandlerConfig) -> Name -> ExpQ
rpcHandler confTrans =
  handler (confTrans defaultRpcHandlerConfig)
  where
    handler (RpcHandlerConfig sync name cmd) funcName = do
      rpcFun <- rpcFunction vimName sync funcName
      rpcCmd <- traverse (rpcCommand vimName funcName) cmd
      listE $ return <$> rpcFun : maybeToList rpcCmd
      where
        vimName = capitalize . fromMaybe (nameBase funcName) $ name

rpcHandlerDef :: Name -> ExpQ
rpcHandlerDef =
  rpcHandler id
