{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Plugin.TH where

import Control.Exception (throw)
import Control.Monad (replicateM, (<=<))
import Data.Aeson (FromJSON, eitherDecodeStrict)
import qualified Data.ByteString as ByteString (intercalate)
import Data.Default (def)
import Data.Either.Combinators (mapLeft)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, maybeToList)
import Data.MessagePack (Object)
import qualified Data.Text as Text (unpack)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..))
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import Neovim.Exceptions (NeovimException(ErrorMessage, ErrorResult))
import Neovim.Plugin.Classes (
  AutocmdOptions(AutocmdOptions),
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
import Ribosome.Plugin.TH.Command (HandlerParams(HandlerParams, handlerHasArgsParam), handlerParams, rpcCommand)
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

functionImplementation :: Name -> Bool -> ExpQ
functionImplementation name hasArgsParam = do
  paramTypes <- functionParamTypes name
  paramNames <- lambdaNames (length paramTypes)
  rpcLambdaWithErrorCase name (argsCase name (listParamsPattern paramNames) paramNames)

rpcFunction :: String -> Synchronous -> Name -> Bool -> ExpQ
rpcFunction name sync funcName hasArgsParam = do
  fun <- functionImplementation funcName hasArgsParam
  [|RpcDef (RpcFunction sync) $((litE (StringL name))) $(return fun)|]

rpcAutocmd :: String -> Name -> Synchronous -> Maybe AutocmdOptions -> String -> ExpQ
rpcAutocmd name funcName sync options event = do
  fun <- functionImplementation funcName False
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
      -- rpcFun <- rpcFunction vimName' sync funcName (handlerHasArgsParam params)
      rpcCmd <- traverse (rpcCommand vimName' funcName params) cmd
      rpcAu <- traverse (rpcAutocmd vimName' funcName sync auOptions) (Text.unpack <$> autocmd)
      -- listE $ return <$> rpcFun : maybeToList rpcCmd <> maybeToList rpcAu
      listE $ return <$> maybeToList rpcCmd <> maybeToList rpcAu
      where
        vimName' = vimName funcName (Text.unpack <$> name)

rpcHandlerDef :: Name -> ExpQ
rpcHandlerDef =
  rpcHandler id
