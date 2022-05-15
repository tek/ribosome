module Ribosome.Api.Tabpage where

import Exon (exon)

import Ribosome.Host.Api.Data (Tabpage)
import Ribosome.Host.Api.Effect (nvimTabpageGetNumber, tabpageIsValid, vimCommand)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Modify (silentBang)

closeTabpage ::
  Member Rpc r =>
  Tabpage ->
  Sem r ()
closeTabpage tabpage =
  whenM (tabpageIsValid tabpage) do
    number <- nvimTabpageGetNumber tabpage
    silentBang do
      vimCommand [exon|tabclose! #{show number}|]
