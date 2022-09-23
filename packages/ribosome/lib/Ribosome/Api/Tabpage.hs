-- |API functions for tabpages.
module Ribosome.Api.Tabpage where

import Exon (exon)

import Ribosome.Host.Api.Data (Tabpage)
import Ribosome.Host.Api.Data (nvimTabpageGetNumber, tabpageIsValid, vimCommand)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Modify (silentBang)

-- |Close a tabpage.
closeTabpage ::
  Member Rpc r =>
  Tabpage ->
  Sem r ()
closeTabpage tabpage =
  whenM (tabpageIsValid tabpage) do
    number <- nvimTabpageGetNumber tabpage
    silentBang do
      vimCommand [exon|tabclose! #{show number}|]
