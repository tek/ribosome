-- | Accessing variables.
module Ribosome.Api.Variable where

import Control.Monad (foldM)
import qualified Data.Map.Strict as Map

import Ribosome.Host.Api.Data (
  nvimBufGetVar,
  nvimGetCurrentBuf,
  nvimGetCurrentTabpage,
  nvimGetCurrentWin,
  nvimGetVar,
  nvimTabpageGetVar,
  nvimWinGetVar,
  )
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import qualified Ribosome.Host.Data.RpcError as RpcError
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)

-- | The nvim scope of a variable.
data VariableScope =
  -- | g:
  GlobalScope
  |
  -- | t:
  TabpageScope
  |
  -- | w:
  WindowScope
  |
  -- | b:
  BufferScope
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)

-- | The reason why a variable could not be read.
data VariableError =
  -- | Variable is undefined in that scope.
  VariableUndefined
  |
  -- | Variable is defined, but couldn't be decoded for the requested type.
  VariableTypeMismatch
  deriving stock (Eq, Show, Generic)

-- | The result of a variable lookup in a scope.
data VariableResult a =
  -- | Variable was successfully decoded.
  VariableDefined a
  |
  -- | Variable could not be found or decoded.
  VariableError VariableError
  deriving stock (Eq, Show, Generic)

-- | The name of a variable, without a scope prefix.
newtype VariableName =
  VariableName Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

-- | The possible variable scopes.
allScopes :: NonEmpty VariableScope
allScopes = [GlobalScope .. BufferScope]

-- | Look up a variable in a specific scope.
variableInScope ::
  MsgpackDecode a =>
  Member (Rpc !! RpcError) r =>
  VariableName ->
  VariableScope ->
  Sem r (VariableResult a)
variableInScope (VariableName name) scope =
  (VariableDefined <$> getter) !! \case
    RpcError.Decode _ -> pure (VariableError VariableTypeMismatch)
    _ -> pure (VariableError VariableUndefined)
  where
    getter = case scope of
      GlobalScope -> nvimGetVar name
      TabpageScope -> do
        tab <- nvimGetCurrentTabpage
        nvimTabpageGetVar tab name
      WindowScope -> do
        win <- nvimGetCurrentWin
        nvimWinGetVar win name
      BufferScope -> do
        buf <- nvimGetCurrentBuf
        nvimBufGetVar buf name

-- | Get the values or error states for a variable in the given scopes, for the given result type.
variableValues ::
  MsgpackDecode a =>
  Member (Rpc !! RpcError) r =>
  VariableName ->
  NonEmpty VariableScope ->
  Sem r (Map VariableScope (VariableResult a))
variableValues name scopes =
  Map.fromList . toList <$> for scopes \ scope -> (scope,) <$> variableInScope name scope

-- | Get the value of a variable in the first scope that defines it with the requested type, or a 'Map' containing the
-- errors for each scope.
findVariable ::
  MsgpackDecode a =>
  Member (Rpc !! RpcError) r =>
  VariableName ->
  NonEmpty VariableScope ->
  Sem r (Either (Map VariableScope VariableError) a)
findVariable name =
  fmap (first Map.fromList) . foldM tryScope (Left mempty)
  where
    tryScope (Right a) _ = pure (Right a)
    tryScope (Left acc) scope =
      variableInScope name scope <&> \case
        VariableDefined a -> Right a
        VariableError err -> Left ((scope, err) : acc)
