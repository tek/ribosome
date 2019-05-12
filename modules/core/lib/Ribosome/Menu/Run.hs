{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Menu.Run where

import Conduit (ConduitT, awaitForever, mapC, mapMC, runConduit, sinkNull, transPipe, yield, (.|))
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.Conduit.Combinators as Conduit (concatMap)
import Data.Conduit.Lift (evalStateC)
import Data.Conduit.TMChan (mergeSources)
import UnliftIO (MonadUnliftIO)

import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Error.Report (processErrorReport')
import Ribosome.Error.Report.Class (errorReport)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuConfig (MenuConfig(MenuConfig))
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(..))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Menu.Nvim (renderNvimMenu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import Ribosome.Menu.Prompt.Data.PromptConsumerUpdate (PromptConsumerUpdate(PromptConsumerUpdate))
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState(..))
import Ribosome.Menu.Prompt.Run (promptC)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Scratch (showInScratch)

updateMenu ::
  Monad m =>
  (MenuUpdate -> m Menu) ->
  Either PromptConsumerUpdate MenuItem ->
  ConduitT (Either PromptConsumerUpdate MenuItem) MenuUpdate (StateT Menu m) ()
updateMenu consumer input =
  yield . update =<< (lift . modifyM') (lift . consumer . update)
  where
    update =
      MenuUpdate (either promptUpdate MenuEvent.NewItems input)
    promptUpdate (PromptConsumerUpdate event prompt) =
      promptEvent event prompt
    promptEvent (PromptEvent.Character a) prompt@(Prompt _ PromptState.Insert _) =
      MenuEvent.PromptChange a prompt
    promptEvent (PromptEvent.Character a) prompt =
      MenuEvent.Mapping a prompt
    promptEvent PromptEvent.Init prompt =
      MenuEvent.Init prompt
    promptEvent PromptEvent.EOF _ =
      MenuEvent.Quit

menuSources ::
  MonadUnliftIO m =>
  PromptConfig m ->
  ConduitT () MenuItem m () ->
  m (ConduitT () (Either PromptConsumerUpdate MenuItem) (ResourceT m) ())
menuSources promptConfig items =
  mergeSources (transPipe lift <$> [promptSource, itemSource]) 64
  where
    promptSource =
      promptC promptConfig .| mapC Left
    itemSource =
      items .| mapC Right

runMenu ::
  MonadUnliftIO m =>
  MenuConfig m ->
  m ()
runMenu (MenuConfig items handle render promptConfig) =
  runResourceT . runConduit . loop =<< menuSources promptConfig items
  where
    loop source =
      source .| transPipe lift pipe
    pipe =
      evalStateC def (awaitForever (updateMenu handle)) .| mapMC render .| sinkNull

data MenuNvimError =
  Rpc RpcError
  |
  Decode DecodeError

deepPrisms ''MenuNvimError

nvimMenu ::
  MonadUnliftIO m =>
  MonadRibo m =>
  Nvim m =>
  ScratchOptions ->
  ConduitT () MenuItem m () ->
  (MenuUpdate -> m Menu) ->
  PromptConfig m ->
  m ()
nvimMenu options items handle promptConfig = do
  scratch <- runExceptT @MenuNvimError $ showInScratch [] options
  either (processErrorReport' "menu" . report) run scratch
  where
    run scratch =
      runMenu $ MenuConfig items handle (render scratch) promptConfig
    render scratch =
      void . runExceptT @MenuNvimError . renderNvimMenu options scratch
    report (Rpc e) =
      errorReport e
    report (Decode e) =
      errorReport e
