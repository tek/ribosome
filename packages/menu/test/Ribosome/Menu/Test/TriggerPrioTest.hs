module Ribosome.Menu.Test.TriggerPrioTest where

import Polysemy.Test (UnitTest, (===))

import Ribosome.Menu.Action (menuAttachPrompt, menuDetachPrompt, menuSuccess)
import Ribosome.Menu.App (notPrompt, onlyPrompt)
import Ribosome.Menu.Data.Filter (substring)
import Ribosome.Menu.Data.MenuResult (MenuResult (Success))
import Ribosome.Menu.Data.State (modal)
import qualified Ribosome.Menu.Effect.MenuTest as MenuTest
import Ribosome.Menu.Effect.MenuTest (sendMapping, sendMappingPrompt)
import Ribosome.Menu.Prompt (menuPrompt)
import qualified Ribosome.Menu.Prompt.Data.Prompt
import Ribosome.Menu.Scratch (menuScratch)
import Ribosome.Menu.Test.Run (testStaticNvimMenu)
import Ribosome.Menu.Test.Util (staticMenuItems)
import Ribosome.Test.Embed (testEmbed_)

test_triggerPrio :: UnitTest
test_triggerPrio = do
  testEmbed_ do
    res <- testStaticNvimMenu its def (modal substring) (menuScratch & #maxSize ?~ 10) app do
      sendMappingPrompt "x"
      sendMappingPrompt "<esc>"
      sendMapping "q"
      MenuTest.result
    Success "detached" === (res <&> (.text))
  where
    app =
      [
        (notPrompt "x", menuAttachPrompt (Just "attached")),
        (onlyPrompt "<esc>", menuDetachPrompt (Just "detached")),
        (notPrompt "q", menuSuccess =<< menuPrompt)
      ]

    its = staticMenuItems (show <$> [1 :: Int .. 10])
