module Ribosome.Menu.Run where

import Conduit (ConduitT, mapC, mapMC, runConduit, sinkNull, transPipe, (.|))
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.Conduit.Combinators as Conduit (concatMap)
import Data.Conduit.TMChan (mergeSources)
import UnliftIO (MonadUnliftIO)

import Ribosome.Menu.Data.MenuConfig (MenuConfig(MenuConfig))
import Ribosome.Menu.Data.MenuContent (MenuContent(MenuContent))
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(..))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Menu.Data.PromptConfig (PromptConfig)
import qualified Ribosome.Menu.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Menu.Prompt (promptC)
import Ribosome.Menu.Prompt.Data.PromptConsumerUpdate (PromptConsumerUpdate(PromptConsumerUpdate))

updateMenu ::
  Monad m =>
  (MenuUpdate -> m ()) ->
  Either PromptConsumerUpdate MenuItem ->
  m (Maybe MenuUpdate)
updateMenu consumer (Left (PromptConsumerUpdate event prompt)) = do
  consumer update
  return (Just update)
  where
    update = MenuUpdate (menuEvent event) (MenuContent []) prompt
    menuEvent (PromptEvent.Character a) =
      MenuEvent.Character a
    menuEvent PromptEvent.EOF =
      MenuEvent.Quit
updateMenu _ (Right _) =
  return Nothing

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
  ∀ m .
  MonadUnliftIO m =>
  MenuConfig m ->
  m ()
runMenu (MenuConfig items handle render promptConfig) =
  runResourceT . runConduit . loop =<< menuSources promptConfig items
  where
    loop source =
      source .| transPipe lift pipe
    pipe =
      mapMC (updateMenu handle) .| Conduit.concatMap maybeToList .| mapMC render .| sinkNull
