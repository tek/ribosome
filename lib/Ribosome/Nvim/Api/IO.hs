{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Nvim.Api.IO where

import Data.MessagePack (Object)
import Neovim (Buffer, Tabpage, Window)
import Prelude

import qualified Ribosome.Nvim.Api.Data as RpcData
import Ribosome.Nvim.Api.GenerateIO (generateIO)

generateIO
