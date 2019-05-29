{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Plugin.TH where

import Data.Default (def)
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as Text (unpack)
import Language.Haskell.TH
import Neovim.Plugin.Classes (
  AutocmdOptions,
  Synchronous(..),
  )

import Ribosome.Data.String (capitalize)
import Ribosome.Plugin.TH.Command (handlerParams, rpcCommand)
import Ribosome.Plugin.TH.Handler (
  RpcDef(RpcDef),
  RpcDefDetail(RpcFunction, RpcAutocmd),
  RpcHandlerConfig(RpcHandlerConfig),
  argsCase,
  defaultRpcHandlerConfig,
  functionParamTypes,
  lambdaNames,
  listParamsPattern,
  rpcLambdaWithErrorCase,
  )

functionImplementation :: Name -> ExpQ
functionImplementation name = do
  paramTypes <- functionParamTypes name
  paramNames <- lambdaNames (length paramTypes)
  rpcLambdaWithErrorCase name (argsCase name (listParamsPattern paramNames) paramNames)

rpcFunction :: String -> Synchronous -> Name -> ExpQ
rpcFunction name sync funcName = do
  fun <- functionImplementation funcName
  [|RpcDef (RpcFunction sync) $((litE (StringL name))) $(return fun)|]

rpcAutocmd :: String -> Name -> Synchronous -> Maybe AutocmdOptions -> String -> ExpQ
rpcAutocmd name funcName sync options event = do
  fun <- functionImplementation funcName
  [|RpcDef (RpcAutocmd event sync (fromMaybe def options)) $((litE (StringL name))) $(return fun)|]

vimName :: Name -> Maybe String -> String
vimName funcName =
  capitalize . fromMaybe (nameBase funcName)

rpcHandler :: (RpcHandlerConfig -> RpcHandlerConfig) -> Name -> ExpQ
rpcHandler confTrans =
  handler (confTrans defaultRpcHandlerConfig)
  where
    handler (RpcHandlerConfig sync name cmd autocmd auOptions) funcName = do
      params <- handlerParams funcName
      rpcFun <- rpcFunction vimName' sync funcName
      rpcCmd <- traverse (rpcCommand vimName' funcName params) cmd
      rpcAu <- traverse (rpcAutocmd vimName' funcName sync auOptions) (Text.unpack <$> autocmd)
      listE $ return <$> rpcFun : maybeToList rpcCmd <> maybeToList rpcAu
      where
        vimName' = vimName funcName (Text.unpack <$> name)

rpcHandlerDef :: Name -> ExpQ
rpcHandlerDef =
  rpcHandler id
