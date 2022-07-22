module Ribosome.Test (
  -- * Introduction
  -- $intro
  module Ribosome.Test.Embed,
  module Ribosome.Test.Data.TestConfig,
  module Ribosome.Test.Error,
  module Ribosome.Test.Ui,
  module Ribosome.Test.Wait,
  module Ribosome.Host.Data.HandlerError,
) where

import Ribosome.Host.Data.HandlerError (resumeHandlerFail, stopHandlerToFail)
import Ribosome.Test.Data.TestConfig
import Ribosome.Test.Embed
import Ribosome.Test.Error
import Ribosome.Test.Ui
import Ribosome.Test.Wait

-- $intro
-- This is the test library for the "Ribosome" Neoivm plugin framework.
--
-- Three different test environments are available:
--
-- - "Ribosome.Test.Embed" runs Neovim as a subprocess and connects over stdio
--
-- - "Ribosome.Test.EmbedTmux" is like "Ribosome.Test.Embed", but provides a tmux server
--
-- - "Ribosome.Test.SocketTmux" runs Neovim in a window in a fresh tmux server, either headless in a pseudo terminal
-- or in an @xterm@ instance
--
-- This module reexports "Ribosome.Test.Embed".
