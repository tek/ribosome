{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Ribosome.Plugin.TH where

import Control.Exception (throw)
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes, fromMaybe)
import Data.MessagePack (Object)
import qualified Data.Set as Set (fromList, member)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..))
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Neovim.Exceptions (NeovimException(ErrorMessage))
import Neovim.Plugin.Classes (
  CommandArguments(..),
  Synchronous(..),
  )

import Language.Haskell.TH

import Ribosome.Data.String (capitalize)
import Ribosome.Msgpack.Decode (fromMsgpack)
import Ribosome.Msgpack.Encode (toMsgpack)

data RpcHandlerConfig =
  RpcHandlerConfig {
    rhcSync :: Synchronous,
    rhcName :: Maybe String
  }
  deriving (Eq, Show)

defaultRpcHandlerConfig :: RpcHandlerConfig
defaultRpcHandlerConfig =
  RpcHandlerConfig Async Nothing

data RpcFunction m =
  RpcFunction {
    rfName :: String,
    rfSync :: Synchronous,
    rfHandler :: [Object] -> m Object
  }

rpcHandler :: RpcHandlerConfig -> Name -> Q Exp
rpcHandler (RpcHandlerConfig sync name) funcName = do
  (_, fun) <- functionImplementation funcName
  [|RpcFunction $((litE (StringL (vimName name)))) $(syncArg) $(return fun)|]
  where
    vimName = capitalize . fromMaybe (nameBase funcName)
    syncArg =
      case sync of
        Sync -> [|Sync|]
        Async -> [|Async|]

rpcHandlerDef :: Name -> Q Exp
rpcHandlerDef =
  rpcHandler defaultRpcHandlerConfig

data ArgType =
  StringyType
  |
  ListOfStringyTypes
  |
  Optional ArgType
  |
  CommandArgumentsType
  |
  OtherType
  deriving (Eq, Ord, Show, Read)

classifyArgType :: Type -> Q ArgType
classifyArgType t = do
  set <- genStringTypesSet
  maybeType <- [t|Maybe|]
  cmdArgsType <- [t|CommandArguments|]
  case t of
    AppT ListT (ConT str) | str `Set.member` set -> return ListOfStringyTypes
    AppT m mt@(ConT _) | m == maybeType -> Optional <$> classifyArgType mt
    ConT str | str `Set.member` set -> return StringyType
    cmd | cmd == cmdArgsType -> return CommandArgumentsType
    _ -> return OtherType
  where
    genStringTypesSet = do
      types <- sequence [[t|String|],[t|ByteString|],[t|Text|]]
      return $ Set.fromList [ n | ConT n <- types ]


functionImplementation :: Name -> Q ([ArgType], Exp)
functionImplementation functionName = do
  fInfo <- reify functionName
  nargs <- mapM classifyArgType $ case fInfo of
    VarI _ functionType _ ->
      determineNumberOfArguments functionType
    x ->
      error $ "Value given to function is (likely) not the name of a function.\n" <> show x
  e <- topLevelCase nargs
  return (nargs, e)
  where
    determineNumberOfArguments :: Type -> [Type]
    determineNumberOfArguments ft = case ft of
      ForallT _ _ t -> determineNumberOfArguments t
      AppT (AppT ArrowT t) r -> t : determineNumberOfArguments r
      _ -> []
    -- \args -> case args of ...
    topLevelCase :: [ArgType] -> Q Exp
    topLevelCase ts = do
      let
        n = length ts
        minLength = length [ () | Optional _ <- reverse ts ]
      args <- newName "args"
      lamE [varP args] (caseE (varE args) (zipWith matchingCase [n,n-1..] [0..minLength] ++ [errorCase]))
    -- _ -> err "Wrong number of arguments"
    errorCase :: Q Match
    errorCase = match wildP
      (normalB [|throw . ErrorMessage . pretty $ "Wrong number of arguments for function: " ++
        $(litE (StringL (nameBase functionName))) |]) []
    -- [x,y] -> case pure add <*> fromObject x <*> fromObject y of ...
    matchingCase :: Int -> Int -> Q Match
    matchingCase n x = do
      vars <- mapM (\_ -> Just <$> newName "x") [1..n]
      let optVars = replicate x (Nothing :: Maybe Name)
      match ((listP . map varP . catMaybes) vars) (
        normalB (caseE (foldl genArgumentCast [|pure $(varE functionName)|] (zip (vars ++ optVars) (repeat [|(<*>)|])))
                [successfulEvaluation, failedEvaluation])) []
    genArgumentCast :: Q Exp -> (Maybe Name, Q Exp) -> Q Exp
    genArgumentCast e = \case
      (Just v,op) ->
        infixE (Just e) op (Just [|fromMsgpack $(varE v)|])
      (Nothing, op) ->
        infixE (Just e) op (Just [|pure Nothing|])
    successfulEvaluation :: Q Match
    successfulEvaluation = newName "action" >>= \action ->
      match (conP (mkName "Right") [varP action]) (normalB [|toMsgpack <$> $(varE action)|]) []
    failedEvaluation :: Q Match
    failedEvaluation = newName "e" >>= \e ->
      match (conP (mkName "Left") [varP e]) (normalB [|throw . ErrorMessage $ ($(varE e) :: Doc AnsiStyle)|]) []

