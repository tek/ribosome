module Ribosome.Api.Process where

import Ribosome.Host.Api.Effect (vimCallFunction)

vimPid =
  vimCallFunction "getpid" []
