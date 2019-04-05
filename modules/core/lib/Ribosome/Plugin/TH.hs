{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Ribosome.Plugin.TH where

import Control.Exception (throw)
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as ByteString (fromString)
import Data.Data (Data)
import Data.Functor ((<&>))
import Data.Functor.Syntax ((<$$>))
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.MessagePack (Object(ObjectString))
import qualified Data.Set as Set (fromList, member)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..))
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import Neovim.Exceptions (NeovimException(ErrorMessage))
import Neovim.Plugin.Classes (
  CommandArguments(..),
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
unfoldFunctionParams a = []

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
  OnlyData Type
  |
  DataPlus Type Int
  deriving (Eq, Show)

errorCase :: Name -> Q Match
errorCase name =
  match wildP (normalB [|throw . ErrorMessage . pretty $ errMsg ++ $(litE (StringL (nameBase name))) |]) []
  where
    errMsg = "Wrong number of arguments for function: " :: String

failedEvaluation :: Q Match
failedEvaluation = do
  e <- newName "e"
  match (conP (mkName "Left") [varP e]) (normalB [|throw . ErrorMessage $ ($(varE e) :: Doc AnsiStyle)|]) []

successfulEvaluation :: Q Match
successfulEvaluation = do
  action <- newName "action"
  match (conP (mkName "Right") [varP action]) (normalB [|toMsgpack <$> $(varE action)|]) []

dispatchCase :: Name -> PatQ -> ExpQ -> Q Match
dispatchCase handlerName params dispatch =
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
  dispatchCase handlerName params dispatch
  where
    dispatch = decodedCallSequence handlerName vars
    vars = varE <$> paramNames

cmdArgsCase :: Name -> [Name] -> Q Match
cmdArgsCase handlerName paramNames =
  argsCase handlerName (listParamsPattern (mkName "_" : paramNames)) paramNames

rpcLambda :: Name -> Q Match -> ExpQ
rpcLambda name match = do
  args <- newName "args"
  lamE [varP args] (caseE (varE args) [match])

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
  rpcLambda name (argsCase name (listParamsPattern paramNames) paramNames)

joinMsgpackStrings :: [Object] -> Either Err Object
joinMsgpackStrings args =
  ObjectString . ByteString.fromString . unwords <$> traverse fromMsgpack args

dataDispatch :: Name -> [Name] -> Name -> ExpQ
dataDispatch handlerName paramNames restName =
  infixApp prims [|(<*>)|] decodedRest
  where
    prims = decodedCallSequence handlerName vars
    vars = varE <$> paramNames
    decodedRest = [|fromMsgpack =<< joinMsgpackStrings rest|]

commandImplementation :: CmdParams -> Name -> ExpQ
commandImplementation ZeroParams name =
  rpcLambda name (cmdArgsCase name [])
commandImplementation (OnlyPrims paramCount) name = do
  paramNames <- lambdaNames paramCount
  rpcLambda name (cmdArgsCase name paramNames)
commandImplementation (DataPlus tpe paramCount) name = do
  paramNames <- lambdaNames paramCount
  restName <- newName "rest"
  rpcLambda name (dispatchCase name (unconsParamsPattern paramNames restName) (dataDispatch name paramNames restName))
commandImplementation (OnlyData tpe) name = do
  restName <- newName "rest"
  rpcLambda name (dispatchCase name (unconsParamsPattern [] restName) (dataDispatch name [] restName))

analyzeCmdParams :: (Type -> Bool) -> [Type] -> CmdParams
analyzeCmdParams isPrim =
  check . reverse
  where
    check [a] | isPrim a =
      OnlyPrims 1
    check [a] =
      OnlyData a
    check (a : rest) | isPrim a =
      OnlyPrims (length rest + 1)
    check (a : rest) =
      DataPlus a (length rest)
    check [] =
      ZeroParams

cmdParams :: Name -> Q CmdParams
cmdParams name = do
  prims <- sequence [[t|String|], [t|ByteString|], [t|Text|], [t|Int|]]
  params <- functionParamTypes name
  return $ analyzeCmdParams (`elem` prims) params

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
  let nargs1 = cmdNargs params
  [|RpcDef (RpcCommand $ mkCommandOptions (nargs1 : opts)) $((litE (StringL name))) $(return fun)|]

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
