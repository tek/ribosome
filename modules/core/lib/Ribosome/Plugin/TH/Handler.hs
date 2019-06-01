{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Plugin.TH.Handler where

import Control.Exception (throw)
import Control.Monad (replicateM)
import Data.Functor ((<&>))
import Data.Maybe (maybeToList)
import Data.MessagePack (Object)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..))
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import Neovim.Exceptions (NeovimException(ErrorMessage))
import Neovim.Plugin.Classes (
  AutocmdOptions(AutocmdOptions),
  CommandOption(..),
  CommandOptions,
  RangeSpecification(..),
  Synchronous(..),
  )

import Ribosome.Msgpack.Decode (fromMsgpack)
import Ribosome.Msgpack.Encode (toMsgpack)

data RpcHandlerConfig =
  RpcHandlerConfig {
    rhcSync :: Synchronous,
    rhcName :: Maybe Text,
    rhcCmd :: Maybe [CommandOption],
    rhcAutocmd :: Maybe Text,
    rhcAutocmdOptions :: Maybe AutocmdOptions
  }
  deriving (Eq, Show)

defaultRpcHandlerConfig :: RpcHandlerConfig
defaultRpcHandlerConfig =
  RpcHandlerConfig Async Nothing Nothing Nothing Nothing

data RpcDefDetail =
  RpcFunction { rfSync :: Synchronous }
  |
  RpcCommand { rcOptions :: CommandOptions }
  |
  RpcAutocmd {
    raEvent :: Text,
    raSync :: Synchronous,
    raOptions :: AutocmdOptions
    }

data RpcDef m =
  RpcDef {
    rdDetail :: RpcDefDetail,
    rdName :: Text,
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

instance Lift AutocmdOptions where
  lift (AutocmdOptions p n g) =
    [|AutocmdOptions p n g|]

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
    _ -> fail $ "rpc handler `" <> show name <> "` is not a function"

errorBody :: Name -> BodyQ
errorBody rpcName =
  normalB [|throw . ErrorMessage . pretty $ ($(errLit) :: String)|]
  where
    errLit =
      litE (StringL errMsg)
    errMsg =
      "Wrong number of arguments for rpc handler: " <> nameBase rpcName

errorCase :: Name -> Q Match
errorCase rpcName =
  match wildP (errorBody rpcName) []

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
    dispatch = decodedCallSequence handlerName paramVars
    paramVars = varE <$> paramNames

rpcLambda :: Q Match -> Maybe (Q Match) -> ExpQ
rpcLambda matchingArgsCase errorCase' = do
  args <- newName "args"
  lamE [varP args] [|$(caseE (varE args) (matchingArgsCase : maybeToList errorCase'))|]

rpcLambdaWithErrorCase :: Name -> Q Match -> ExpQ
rpcLambdaWithErrorCase funcName matchingArgsCase =
  rpcLambda matchingArgsCase $ Just (errorCase funcName)

rpcLambdaWithoutErrorCase :: Q Match -> ExpQ
rpcLambdaWithoutErrorCase matchingArgsCase =
  rpcLambda matchingArgsCase Nothing

listParamsPattern :: [Name] -> PatQ
listParamsPattern =
  listP . (varP <$>)

lambdaNames :: Int -> Q [Name]
lambdaNames count =
  replicateM count (newName "a")