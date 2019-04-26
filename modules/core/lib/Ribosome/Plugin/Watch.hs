module Ribosome.Plugin.Watch where

import Control.Lens (Lens')
import qualified Control.Lens as Lens (at)
import Control.Monad.DeepError (catchAt)
import qualified Data.Map as Map (toList)
import Data.Map.Strict (Map)
import Data.MessagePack (Object(ObjectNil))

import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, pluginInternalL, pluginInternalModifyL)
import Ribosome.Control.Ribosome (RibosomeInternal)
import qualified Ribosome.Control.Ribosome as RibosomeInternal (watchedVariables)
import Ribosome.Nvim.Api.IO (vimGetVar)
import Ribosome.Nvim.Api.RpcCall (RpcError)

data WatchedVariable m =
  WatchedVariable {
    wvName :: Text,
    wvHandler :: Object -> m ()
  }

watchedVariables :: Map Text (Object -> m ()) -> [WatchedVariable m]
watchedVariables =
  fmap create . Map.toList
  where
    create (name, handler) = WatchedVariable name handler

storedVarLens :: Text -> Lens' RibosomeInternal (Maybe Object)
storedVarLens name =
  RibosomeInternal.watchedVariables . Lens.at name

runHandler ::
  MonadRibo m =>
  WatchedVariable m ->
  Object ->
  m ()
runHandler (WatchedVariable name handler) new = do
  pluginInternalModifyL (storedVarLens name) (const (Just new))
  handler new

compareVar ::
  MonadRibo m =>
  WatchedVariable m ->
  Maybe Object ->
  Object ->
  m ()
compareVar _ (Just old) new | old == new =
  return ()
compareVar wv _ new =
  runHandler wv new

checkVar ::
  MonadRibo m =>
  NvimE e m =>
  WatchedVariable m ->
  m ()
checkVar wv@(WatchedVariable name _) = do
  old <- pluginInternalL (storedVarLens name)
  new <- catchAt @RpcError recover (Right <$> vimGetVar name)
  traverse_ (compareVar wv old) new
  where
  recover _ = return (Left ())

handleWatcherRequest ::
  MonadRibo m =>
  NvimE e m =>
  [WatchedVariable m] ->
  [Object] ->
  m Object
handleWatcherRequest variables _ =
  ObjectNil <$ traverse_ checkVar variables
