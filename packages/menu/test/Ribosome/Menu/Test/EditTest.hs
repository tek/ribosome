module Ribosome.Menu.Test.EditTest where

import Polysemy.Test (UnitTest, assertEq)

import Ribosome.Api.Buffer (bufferContent)
import qualified Ribosome.Data.ScratchState
import Ribosome.Menu.Action (MenuWidget, menuAttachPrompt, menuRenderIndex)
import Ribosome.Menu.App (defaultHandlers, promptControl)
import Ribosome.Menu.Class.MenuMode (MenuMode (matcher))
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem))
import Ribosome.Menu.Data.State (Modal, modal)
import qualified Ribosome.Menu.Effect.MenuTest as MenuTest
import Ribosome.Menu.Effect.MenuTest (sendMappingPrompt, sendPromptRender)
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Items (modifyFocus, withFocus')
import Ribosome.Menu.Test.Run (testStaticNvimMenu)
import qualified Ribosome.Menu.Prompt.Data.Prompt
import Ribosome.Menu.Prompt.Data.Prompt (PromptText (PromptText))
import Ribosome.Menu.Scratch (menuScratch)
import Ribosome.Menu.Test.Util (staticMenuItems)
import Ribosome.Test.Embed (testEmbed_)

data NoFilter =
  NoFilter
  deriving stock (Eq, Show, Ord, Generic)

instance MenuMode Text NoFilter where
  matcher NoFilter _ _ e = Just (0, e)

edit :: MenuWidget (Modal NoFilter Text) r a
edit = do
  old <- ask
  withFocus' \ a ->
    menuAttachPrompt (Just (old.prompt & #text .~ PromptText a))

promptUpdate :: MenuWidget (Modal NoFilter Text) r a
promptUpdate = do
  PromptText new <- asks (.prompt.text)
  modifyFocus \ e -> e & #item .~ MenuItem new new [new]
  menuRenderIndex

test_editMode :: UnitTest
test_editMode =
  testEmbed_ do
    testStaticNvimMenu its def (modal NoFilter) (menuScratch & #maxSize ?~ 4) maps do
      scr <- MenuUi.itemsScratch
      promptScratch <- MenuUi.promptScratch
      sendMappingPrompt "e"
      sendPromptRender "sub"
      sendMappingPrompt "<esc>"
      assertEq ["sub"] =<< bufferContent promptScratch.buffer
      assertEq ["5", "6", "7", "sub"] =<< bufferContent scr.buffer
      MenuTest.quit
  where
    maps = promptControl promptUpdate defaultHandlers <> [("e", edit)]

    its = staticMenuItems (reverse (show <$> [1 :: Int .. 8]))
