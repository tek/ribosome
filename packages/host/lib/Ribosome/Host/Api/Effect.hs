{-# options_haddock prune #-}

module Ribosome.Host.Api.Effect where

import qualified Ribosome.Host.Api.Data as RpcData
import Ribosome.Host.Api.Data (Buffer, Tabpage, Window)
import Ribosome.Host.TH.Api.GenerateEffect (generateEffect)

generateEffect
